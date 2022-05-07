import argparse
import configparser
import os
import shutil
import subprocess
import sys
import tempfile
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from functools import cache, cached_property
from pathlib import Path
from typing import Any, Callable, Iterable, Literal, Optional, TypeVar, Union, cast

import requests
import tqdm  # type: ignore
import yaml


def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs="*", help="The component to install")
    return parser


OS = Literal["linux", "darwin"]

CURRENT_OS: OS = cast(OS, sys.platform)

T = TypeVar("T")


def with_os(*, linux: T, macos: T) -> T:
    if CURRENT_OS == "linux":
        return linux
    elif CURRENT_OS == "darwin":
        return macos
    else:
        raise ValueError(f"Unexpected OS {CURRENT_OS}.")


Some = Optional[Union[T, list[T]]]

ApplicableArg = Some[OS]


def unsome(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


class Package(metaclass=ABCMeta):
    applicable: Optional[list[OS]]

    def __init__(self, applicable: ApplicableArg = None) -> None:
        self.applicable = unsome(applicable)

    @abstractmethod
    def package_name(self) -> str:
        pass

    @abstractmethod
    def get_remote_version(self) -> str:
        pass

    @cached_property
    def remote_version(self) -> str:
        return self.get_remote_version()

    @abstractmethod
    def local_version(self) -> Optional[str]:
        pass

    def is_installed(self) -> bool:
        return self.remote_version == self.local_version()

    @abstractmethod
    def install(self) -> None:
        pass

    def ensure(self):
        if self.applicable is not None and CURRENT_OS not in self.applicable:
            return
        if self.is_installed():
            return
        self.install()


def run(*args, **kwargs) -> subprocess.CompletedProcess:
    return subprocess.run(args, check=True, **kwargs)


class Installer(metaclass=ABCMeta):
    @abstractmethod
    def install(self, *packages: str) -> None:
        pass

    @abstractmethod
    def is_installed(self, package: str) -> bool:
        pass


class Brew(Installer):
    def install(self, *packages: str) -> None:
        run("brew", "install", *packages)

    def is_installed(self, package: str) -> bool:
        raise NotImplementedError()


class DNF(Installer):
    def install(self, *packages: str) -> None:
        run("sudo", "dnf", "install", "-y", *packages)

    def is_installed(self, package: str) -> bool:
        return (
            subprocess.run(
                ["rpm", "-q", "--whatprovides", package],
                check=False,
                stdout=subprocess.DEVNULL,
            ).returncode
            == 0
        )


class Apt(Installer):
    def install(self, *packages: str) -> None:
        run("sudo", "apt", "install", "--yes", *packages)

    def is_installed(self, package: str) -> bool:
        return (
            subprocess.run(
                ["dpkg", "-s", package],
                check=False,
                stdout=subprocess.DEVNULL,
            ).returncode
            == 0
        )


def linux_installer() -> Installer:
    if shutil.which("dnf"):
        return DNF()
    elif shutil.which("apt"):
        return Apt()
    else:
        raise NotImplementedError("Cannot find a package manager.")


INSTALLER = with_os(linux=linux_installer, macos=lambda: Brew())()


class InstallerPackage(Package):
    def __init__(self, *, name: str, **kwargs) -> None:
        self.name = name
        super().__init__(**kwargs)

    def package_name(self) -> str:
        return self.name

    def get_remote_version(self) -> str:
        return "repository"

    def local_version(self) -> Optional[str]:
        if INSTALLER.is_installed(self.name):
            return "repository"
        else:
            return None

    def install(self):
        INSTALLER.install(self.name)


def home(*path: str) -> str:
    return os.path.join(os.environ["HOME"], *path)


def local(*path: str) -> str:
    return home(".local", *path)


def makedirs(path: str) -> None:
    os.makedirs(path, exist_ok=True)


def make_executable(path: str) -> None:
    run("chmod", "+x", path)


def icon_name(app_path: str) -> Optional[str]:
    app = configparser.ConfigParser()
    app.read(app_path)
    return app["Desktop Entry"].get("Icon")


class ManualPackage(Package, metaclass=ABCMeta):
    binaries: list[str]
    apps: list[str]
    fonts: list[str]

    def __init__(
        self,
        *,
        as_global: bool = False,
        binary: Some[str] = None,
        binary_wrapper: bool = False,
        app: Some[str] = None,
        font: Some[str] = None,
        **kwargs,
    ) -> None:
        self.as_global = as_global
        self.binaries = unsome(binary) or []
        self.binary_wrapper = binary_wrapper
        self.apps = unsome(app) or []
        self.fonts = unsome(font) or []
        super().__init__(**kwargs)

    @abstractmethod
    def get_remote_version(self) -> str:
        pass

    @abstractmethod
    def local_version(self) -> Optional[str]:
        pass

    def local(self, *path: str) -> str:
        if self.as_global:
            return os.path.join("/usr/local", *path)
        else:
            return local(*path)

    def link(self, source: str, target: str, wrapper: bool = False) -> None:
        target_dir = os.path.dirname(target)
        if self.as_global:
            run("sudo", "mkdir", "-p", target_dir)
        else:
            makedirs(target_dir)
        if os.path.lexists(target):
            if self.as_global:
                run("sudo", "rm", target)
            else:
                os.unlink(target)
        if wrapper:
            with open(target, "w") as wrapper_file:
                print(f'#!/bin/sh\nexec "{source}" "$@"', file=wrapper_file)
        else:
            if self.as_global:
                run("sudo", "ln", "-s", source, target)
            else:
                os.symlink(source, target)

    @abstractmethod
    def binary_path(self, binary: str) -> str:
        pass

    def install_binary(self, name: str) -> None:
        path = self.binary_path(name)
        target = self.local("bin", name)
        self.link(path, target, wrapper=self.binary_wrapper)

    @abstractmethod
    def app_path(self, name: str) -> str:
        pass

    def icon_directory(self) -> Optional[str]:
        return None

    def install_app(self, name: str) -> None:
        path = self.app_path(name)
        target = self.local("share", "applications", f"{name}.desktop")
        self.link(path, target)
        icon_directory = self.icon_directory()
        if icon_directory:
            icons_target = self.local("share", "icons")
            icon = icon_name(path)
            if icon:
                for icon_path in Path(icon_directory).rglob(f"{icon}.*"):
                    package_icon = str(icon_path)
                    target = os.path.join(
                        icons_target, icon_path.relative_to(icon_directory)
                    )
                    self.link(package_icon, target)

    @abstractmethod
    def font_path(self, name: str) -> str:
        pass

    def install_font(self, name: str) -> None:
        font_dir = with_os(
            linux=self.local("share", "fonts"), macos=home("Library", "Fonts")
        )
        makedirs(font_dir)
        source = self.font_path(name)
        target = os.path.join(font_dir, name)
        self.link(source, target)
        if shutil.which("fc-cache"):
            run("fc-cache", "-f", font_dir)

    def install(self) -> None:
        for binary in self.binaries:
            self.install_binary(binary)
        for app in self.apps:
            self.install_app(app)
        for font in self.fonts:
            self.install_font(font)


class ArchivePackage(ManualPackage, metaclass=ABCMeta):
    def __init__(
        self,
        *,
        raw: Union[bool, str] = False,
        raw_executable: bool = False,
        strip: int = 0,
        **kwargs,
    ) -> None:
        self.raw = raw
        self.raw_executable = raw_executable
        self.strip = strip
        super().__init__(**kwargs)

    @abstractmethod
    def archive_url(self) -> str:
        pass

    def package_directory(self) -> str:
        result = local(f"{self.package_name().replace('/', '--')}.app")
        makedirs(result)
        return result

    def untar(self, source: str, *extra: str) -> None:
        run(
            "tar",
            "-x",
            "--strip",
            str(self.strip),
            "-C",
            self.package_directory(),
            *extra,
            "-f",
            source,
        )

    def extract(self, url: str, source: str) -> None:
        if self.raw:
            if isinstance(self.raw, str):
                filename = self.raw
            else:
                filename = url.rsplit("/", 1)[-1]
            target = os.path.join(self.package_directory(), filename)
            run("cp", source, target)
            if self.raw_executable:
                make_executable(target)
        elif ".tar" in url:
            if ".gz" in url:
                self.untar(source, "-z")
            elif ".bz2" in url:
                self.untar(source, "-j")
            else:
                self.untar(source)
        elif ".tgz" in url:
            self.untar(source, "-z")
        elif ".txz" in url:
            self.untar(source, "-J")
        else:
            raise ValueError(f"Unknown archive format: {url}")

    def binary_path(self, binary: str) -> str:
        paths: list[list[str]] = [[], ["bin"]]
        for relative_path in paths:
            candidate = os.path.join(self.package_directory(), *relative_path, binary)
            if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
                return candidate
        raise ValueError(f"Cannot find {binary} in {self.package_directory()}.")

    def app_path(self, name: str) -> str:
        candidate = os.path.join(
            self.package_directory(), "share", "applications", f"{name}.desktop"
        )
        if os.path.isfile(candidate):
            return candidate
        raise ValueError(
            f"Cannot find application '{name}' in {self.package_directory()}."
        )

    def icon_directory(self) -> Optional[str]:
        candidate = os.path.join(self.package_directory(), "share", "icons")
        if os.path.isdir(candidate):
            return candidate
        return None

    def font_path(self, name: str) -> str:
        candidate = os.path.join(self.package_directory(), name)
        if os.path.isfile(candidate):
            return candidate
        raise ValueError(f"Cannot find font '{name}' in {self.package_directory()}.")

    def install(self):
        url = self.archive_url()
        with tempfile.NamedTemporaryFile() as archive_file:
            run("curl", "-sSL", url, stdout=archive_file)
            self.extract(url, archive_file.name)
        super().install()


class URLPackage(ArchivePackage):
    def __init__(self, *, url: str, **kwargs) -> None:
        self.url = url
        super().__init__(**kwargs)

    def archive_url(self) -> str:
        return self.url

    def package_name(self):
        parts = self.url.split("/")
        while True:
            if len(parts) == 0:
                raise ValueError(f"Cannot parse package name from {self.url}.")
            elif parts[0] in ("", "https:"):
                parts.pop(0)
            elif parts[0] == "github.com":
                return "/".join(parts[1:2])
            else:
                return parts[0]


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


def parse_package(package: Any) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected: {package}.")
    if "name" in package:
        return InstallerPackage(**package)
    elif "repo" in package:
        return GitHubPackage(**package)
    elif "url" in package:
        return URLPackage(**package)
    else:
        raise ValueError(
            f"Either 'name', 'repo' or 'url' must be present, got: {package}."
        )


def load_packages(component: str) -> list[Package]:
    with open(
        os.path.join(os.path.dirname(__file__), "packages", f"{component}.yaml")
    ) as f:
        packages = yaml.safe_load(f)
        return list(map(parse_package, packages))


def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = [
        package for component in components for package in load_packages(component)
    ]
    with tqdm.tqdm(total=len(packages)) as progress:
        for package in packages:
            progress.set_description(package.package_name())
            package.ensure()
            progress.update(1)


if __name__ == "__main__":
    main()
