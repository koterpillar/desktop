import argparse
from ctypes import Union
import os
import platform
import sys
import subprocess
from typing import Any, Literal, Optional, Sequence

import yaml

def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs='*', help="The component to install")
    return parser

OS = Literal['linux', 'darwin']

CURRENT_OS: OS = sys.platform

ApplicableArg = Optional[Union[OS, Sequence[OS]]]

class Package:
    applicable: Optional[frozenset[OS]]

    def __init__(self, applicable: ApplicableArg) -> None:
        if applicable is None:
            self.applicable = None
        elif isinstance(applicable, str):
            self.applicable = frozenset([applicable])
        else:
            self.applicable = frozenset(applicable)

    def install(self):
        raise NotImplementedError

    def ensure(self):
        if self.applicable is not None and CURRENT_OS not in self.applicable:
            return
        self.install()

class InstallerPackage(Package):
    def __init__(self, *, applicable: ApplicableArg = None, name: str) -> None:
        self.name = name
        super().__init__(applicable=applicable)

    def install(self):
        if CURRENT_OS == 'linux':
            distro = platform.freedesktop_os_release()['ID']
            if distro == 'fedora':
                installer = ['sudo', 'dnf', 'install']
            elif distro == 'debian':
                installer = ['sudo', 'apt', 'install', '--yes']
            else:
                raise NotImplementedError(f"Unknown Linux distribution: {distro}.")
        elif CURRENT_OS == 'darwin':
            installer = ['brew', 'install']
        else:
            raise NotImplementedError(f"Unknown platform {CURRENT_OS}.")
        subprocess.run([*installer, self.name], check=True)

class GitHubPackage(Package):
    def __init__(self, *, applicable: ApplicableArg, repo: str) -> None:
        self.repo = repo
        super().__init__(applicable=applicable)

def parse_package(package: Any) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected: {package}.")
    if 'name' in package:
        return InstallerPackage(**package)
    elif 'repo' in package:
        return GitHubPackage(**package)
    else:
        raise ValueError(f"Either 'name' or 'repo' must be present, got: {package}.")

def load_packages(component: str) -> list[Package]:
    with open(os.path.join(os.path.dirname(__file__), 'packages', f'{component}.yaml')) as f:
        packages = yaml.safe_load(f)
        return list(map(parse_package, packages))

def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {'base'}
    for component in components:
        for package in load_packages(component):
            package.ensure()

if __name__ == '__main__':
    main()
