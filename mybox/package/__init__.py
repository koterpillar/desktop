import os
from typing import Any

import yaml

from ..utils import ROOT_DIR
from .base import Package
from .clone import Clone
from .github import GitHubPackage
from .system import SystemPackage
from .url import URLPackage


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


def load_packages(component: str) -> list[Package]:
    with open(os.path.join(ROOT_DIR, "packages", f"{component}.yaml")) as f:
        packages = yaml.safe_load(f)
        return list(map(parse_package, packages))
