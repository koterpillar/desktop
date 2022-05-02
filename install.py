import argparse
from dataclasses import dataclass
import os
import platform
import shutil
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

def with_os(*, linux: T, macos: T) -> T:
    if CURRENT_OS == 'linux':
        return linux
    elif CURRENT_OS == 'darwin':
        return macos
    else:
        raise ValueError(f"Unexpected OS {CURRENT_OS}.")

Some = Optional[Union[T, Sequence[T]]]

ApplicableArg = Some[OS]

def unsome(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]

class Package:
    applicable: Optional[frozenset[OS]]

    def __init__(self, applicable: ApplicableArg = None) -> None:
        self.applicable = unsome(applicable)

    def install(self):
        raise NotImplementedError

    def package_name(self):
        raise NotImplementedError

    def ensure(self):
        if self.applicable is not None and CURRENT_OS not in self.applicable:
            return
        self.install()

def run(*args, **kwargs) -> None:
    subprocess.run(args, check=True, **kwargs)

class InstallerPackage(Package):
    def __init__(self, *, name: str, **kwargs) -> None:
        self.name = name
        super().__init__(**kwargs)

    def linux_installer(self) -> list[str]:
        distro = platform.freedesktop_os_release()['ID']
        if distro == 'fedora':
            return ['sudo', 'dnf', 'install', '-y']
        elif distro == 'debian':
            return ['sudo', 'apt', 'install', '--yes']
        else:
            raise NotImplementedError(f"Unknown Linux distribution: {distro}.")

    def install(self):
        installer = with_os(
            linux=self.linux_installer,
            macos=lambda: ['brew', 'install'],
        )()
        run(*installer, self.name)


def home(*path: str) -> str:
    return os.path.join(os.environ['HOME'], *path)

def local(*path: str) -> str:
    return home('.local', *path)

def symlink(source: str, target: str) -> None:
    if os.path.lexists(target):
        os.unlink(target)
    os.symlink(source, target)

class LocalPackage(Package):
    binaries: list[str]
    fonts: list[str]
    def __init__(self, *, binary: Some[str] = None, font: Some[str] = None, **kwargs) -> None:
        self.binaries = unsome(binary) or []
        self.fonts = unsome(font) or []
        super().__init__(**kwargs)

    def binary_path(self, binary: str) -> str:
        raise NotImplementedError()

    def install_binary(self, name: str) -> None:
        os.makedirs(local('bin'), exist_ok=True)
        target = local('bin', name)
        symlink(self.binary_path(name), target)

    def install_font(self, name: str) -> None:
        font_dir = with_os(linux=local('share', 'fonts'), macos=home('Library', 'Fonts'))
        os.makedirs(font_dir, exist_ok=True)
        source = os.path.join(self.package_directory(), name)
        target = os.path.join(font_dir, name)
        symlink(source, target)
        if shutil.which('fc-cache'):
            run('fc-cache', '-f', font_dir)

    def install(self) -> None:
        for binary in self.binaries:
            self.install_binary(binary)
        for font in self.fonts:
            self.install_font(font)


class CargoPackage(LocalPackage):
    def __init__(self, *, cargo: str, **kwargs) -> None:
        self.name = cargo
        super().__init__(**kwargs)

    def binary_path(self, binary: str) -> str:
        return home('.cargo', 'bin', binary)

    def install(self):
        run('cargo', 'install', self.name)
        super().install()

class ArchivePackage(LocalPackage):
    def __init__(self, *, raw: Union[bool, str] = False, raw_executable: bool = False, strip: int = 0, **kwargs) -> None:
        self.raw = raw
        self.raw_executable = raw_executable
        self.strip = strip
        super().__init__(**kwargs)

    def archive_url(self) -> str:
        raise NotImplementedError()

    def package_directory(self) -> str:
        result = local(f'{self.package_name()}.app')
        os.makedirs(result, exist_ok=True)
        return result

    def untar(self, source: str, *extra: str) -> Sequence[str]:
        run('tar', '-x', '--strip', str(self.strip), '-C', self.package_directory(), *extra, '-f', source)

    def extract(self, url: str, source: str) -> None:
        if self.raw:
            if isinstance(self.raw, str):
                filename = self.raw
            else:
                filename = url.rsplit('/', 1)[-1]
            target = os.path.join(self.package_directory(), filename)
            run('cp', source, target)
            if self.raw_executable:
                run('chmod', '+x', target)
        elif '.tar' in url:
            if '.gz' in url:
                return self.untar(source, '-z')
            elif '.bz2' in url:
                return self.untar(source, '-j')
            else:
                return self.untar(source)
        elif '.tgz' in url:
            return self.untar(source, '-z')
        else:
            raise ValueError(f"Unknown archive format: {url}")

    def binary_path(self, binary: str) -> str:
        for relative_path in [[], ['bin']]:
            candidate = os.path.join(self.package_directory(), *relative_path, binary)
            if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
                return candidate
        raise ValueError(f"Cannot find {binary} in {self.package_directory()}.")

    def install(self):
        url = self.archive_url()
        with tempfile.NamedTemporaryFile() as archive_file:
            run('curl', '-sSL', url, stdout=archive_file)
            self.extract(url, archive_file.name)
        super().install()

class URLPackage(ArchivePackage):
    def __init__(self, *, url: str, **kwargs) -> None:
        self.url = url
        super().__init__(**kwargs)

    def archive_url(self) -> str:
        return self.url

    def package_name(self):
        return self.url.split('/')[2]

@dataclass
class GitHubRelease:
    name: str
    url: str

class GitHubPackage(ArchivePackage):
    prefixes: list[str]
    suffixes: list[str]
    excludes: list[str]

    def __init__(self, *, repo: str, prefix: Some[str] = None, suffix: Some[str] = None, exclude: Some[str] = None, **kwargs) -> None:
        self.repo = repo
        self.prefixes = unsome(prefix) or []
        self.suffixes = unsome(suffix) or []
        self.excludes = unsome(exclude) or []
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
        for exclude in self.excludes:
            yield lambda name: exclude not in name
        os_hints = with_os(
            linux=['linux', 'gnu'],
            macos=['macos', 'darwin', 'osx'],
        )
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
    elif 'url' in package:
        return URLPackage(**package)
    elif 'cargo' in package:
        return CargoPackage(**package)
    else:
        raise ValueError(f"Either 'name', 'repo', 'url' or 'cargo' must be present, got: {package}.")

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
