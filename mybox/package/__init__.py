from typing import Any

from .base import Package
from .clone import Clone
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
    elif "clone" in package:
        return Clone(**package)
    else:
        raise ValueError(
            f"Either 'name', 'repo' 'url' or 'clone' must be present, got: {package}."
        )
