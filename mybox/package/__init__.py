from typing import Any

from .base import Package
from .github import GitHubPackage
from .manual import URLPackage
from .system import SystemPackage


def parse_package(package: Any) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected: {package}.")
    if "name" in package:
        return SystemPackage(**package)
    elif "repo" in package:
        return GitHubPackage(**package)
    elif "url" in package:
        return URLPackage(**package)
    else:
        raise ValueError(
            f"Either 'name', 'repo' or 'url' must be present, got: {package}."
        )
