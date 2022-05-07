import configparser
import os
import shutil
import tempfile
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from functools import cache
from pathlib import Path
from typing import Callable, Iterable

import requests

from ..utils import *
from .manual import ArchivePackage


@dataclass
class GitHubReleaseArtifact:
    name: str
    url: str


@dataclass
class GitHubRelease:
    tag_name: str
    assets: list[GitHubReleaseArtifact]


class GitHubPackage(ArchivePackage):
    prefixes: list[str]
    suffixes: list[str]
    excludes: list[str]

    def __init__(
        self,
        *,
        repo: str,
        prefix: Some[str] = None,
        suffix: Some[str] = None,
        exclude: Some[str] = None,
        **kwargs,
    ) -> None:
        self.repo = repo
        self.prefixes = unsome(prefix) or []
        self.suffixes = unsome(suffix) or []
        self.excludes = unsome(exclude) or []
        super().__init__(**kwargs)

    @cache
    def latest_release(self) -> GitHubRelease:
        latest = requests.get(
            f"https://api.github.com/repos/{self.repo}/releases/latest"
        ).json()
        return GitHubRelease(
            tag_name=latest["tag_name"],
            assets=[
                GitHubReleaseArtifact(
                    name=result["name"], url=result["browser_download_url"]
                )
                for result in latest["assets"]
            ],
        )

    def filters(self) -> Iterable[Callable[[str], bool]]:
        for prefix in self.prefixes:
            yield lambda name: name.startswith(prefix)
        for suffix in self.suffixes:
            yield lambda name: name.endswith(suffix)
        for exclude in self.excludes:
            yield lambda name: exclude not in name
        for hint in [".tar.gz"]:
            yield lambda name: hint in name.lower()
        os_hints = with_os(
            linux=["linux", "gnu"],
            macos=["macos", "darwin", "osx"],
        )
        for os_hint in os_hints:
            yield lambda name: os_hint in name.lower()
        arch_hints = ["x86_64", "amd64"]
        for arch_hint in arch_hints:
            yield lambda name: arch_hint in name.lower()

    def artifact(self) -> GitHubReleaseArtifact:
        candidates = self.latest_release().assets
        for filter_fn in self.filters():
            if len(candidates) == 1:
                return candidates[0]
            new_candidates = [
                candidate for candidate in candidates if filter_fn(candidate.name)
            ]
            if new_candidates:
                candidates = new_candidates
            if len(candidates) == 1:
                return candidates[0]
        raise ValueError(f"Cannot choose between: {candidates}")

    def archive_url(self) -> str:
        return self.artifact().url

    def package_name(self) -> str:
        return self.repo

    def get_remote_version(self) -> str:
        return self.latest_release().tag_name
