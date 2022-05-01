import argparse
from dataclasses import dataclass
import os
import platform
import sys
import subprocess
import tempfile
from typing import Any, Callable, Iterable, Literal, Optional, Sequence, TypeVar, Union

import requests
import yaml

def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs='*', help="The component to install")
    return parser

OS = Literal['linux', 'darwin']

CURRENT_OS: OS = sys.platform

T = TypeVar('T')

Some = Optional[Union[T, Sequence[T]]]

ApplicableArg = Some[OS]

class Package:
    applicable: Optional[frozenset[OS]]

    def __init__(self, applicable: ApplicableArg = None) -> None:
        if applicable is None:
            self.applicable = None
        elif isinstance(applicable, str):
            self.applicable = frozenset([applicable])
        else:
            self.applicable = frozenset(applicable)

    def install(self):
        raise NotImplementedError

    def package_name(self):
        raise NotImplementedError

    def ensure(self):
        if self.applicable is not None and CURRENT_OS not in self.applicable:
            return
        self.install()

class InstallerPackage(Package):
    def __init__(self, *, name: str, **kwargs) -> None:
        self.name = name
        super().__init__(**kwargs)

    def install(self):
        if CURRENT_OS == 'linux':
            distro = platform.freedesktop_os_release()['ID']
            if distro == 'fedora':
                installer = ['sudo', 'dnf', 'install', '-y']
            elif distro == 'debian':
                installer = ['sudo', 'apt', 'install', '--yes']
            else:
                raise NotImplementedError(f"Unknown Linux distribution: {distro}.")
        elif CURRENT_OS == 'darwin':
            installer = ['brew', 'install']
        else:
            raise NotImplementedError(f"Unknown platform {CURRENT_OS}.")
        subprocess.run([*installer, self.name], check=True)


def local(*path: str) -> str:
    return os.path.join(os.environ['HOME'], '.local', *path)


class LocalPackage(Package):
    binaries: list[str]
    def __init__(self, *, binary: Union[str, list[str]], **kwargs) -> None:
        if isinstance(binary, str):
            self.binaries = [binary]
        else:
            self.binaries = binary
        super().__init__(**kwargs)

    def package_directory(self) -> str:
        result = local(f'{self.package_name()}.app')
        os.makedirs(result, exist_ok=True)
        return result

    def find_binary(self, binary: str) -> str:
        for relative_path in [[], ['bin']]:
            candidate = os.path.join(self.package_directory(), *relative_path, binary)
            if os.path.exists(candidate):
                return candidate
        raise ValueError(f"Cannot find {binary} in {self.package_directory()}.")

    def makelinks(self) -> None:
        for binary in self.binaries:
            source = self.find_binary(binary)
            os.symlink(source, local('bin', binary))

class ArchivePackage(LocalPackage):
    def __init__(self, *, strip: int = 0, **kwargs) -> None:
        self.strip = strip
        super().__init__(**kwargs)

    def archive_url(self) -> str:
        raise NotImplementedError()

    def tar_args(self, *extra: str) -> Sequence[str]:
        return ['tar', '-x', '--strip', str(self.strip), '-C', self.package_directory(), *extra, '-f']

    def extractor(self, url: str) -> Sequence[str]:
        if '.tar' in url:
            if '.gz' in url:
                return self.tar_args('-z')
            elif '.bz2' in url:
                return self.tar_args('-j')
            else:
                return self.tar_args()
        elif '.tgz' in url:
            return self.tar_args('-z')
        else:
            raise ValueError(f"Unknown archive format: {url}")

    def install(self):
        url = self.archive_url()
        with tempfile.NamedTemporaryFile() as archive_file:
            subprocess.run(['curl', '-sSL', url], stdout=archive_file, check=True)
            subprocess.run([*self.extractor(url), archive_file.name], check=True)
        self.makelinks()

@dataclass
class GitHubRelease:
    name: str
    url: str

class GitHubPackage(ArchivePackage):
    prefixes: list[str]
    suffixes: list[str]

    def __init__(self, *, repo: str, prefix: Some[str] = None, suffix: Some[str] = None, **kwargs) -> None:
        self.repo = repo
        self.prefixes = prefix or []
        self.suffixes = suffix or []
        super().__init__(**kwargs)

    def releases(self) -> list[GitHubRelease]:
        latest = requests.get(f'https://api.github.com/repos/{self.repo}/releases/latest').json()
        results = latest['assets']
        return [
            GitHubRelease(name=result['name'], url=result['browser_download_url'])
            for result in results
        ]

    def filters(self) -> Iterable[Callable[[str], bool]]:
        for prefix in self.prefixes:
            yield lambda name: name.startswith(prefix)
        for suffix in self.suffixes:
            yield lambda name: name.endswith(suffix)
        if CURRENT_OS == 'linux':
            os_hints = ['linux', 'gnu']
        elif CURRENT_OS == 'macos':
            os_hints = ['macos', 'darwin', 'osx']
        for os_hint in os_hints:
            yield lambda name: os_hint in name.lower()
        arch_hints = ['x86_64', 'amd64']
        for arch_hint in arch_hints:
            yield lambda name: arch_hint in name.lower()

    def release(self) -> GitHubRelease:
        candidates = self.releases()
        for filter_fn in self.filters():
            if len(candidates) == 1:
                return candidates[0]
            new_candidates = [candidate for candidate in candidates if filter_fn(candidate.name)]
            if new_candidates:
                candidates = new_candidates
            if len(candidates) == 1:
                return candidates[0]
        raise ValueError(f"Cannot choose between: {candidates}")

    def archive_url(self) -> str:
        return self.release().url

    def package_name(self) -> str:
        return self.repo.rsplit('/', 1)[-1]

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
