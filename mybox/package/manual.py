import configparser
import os
import shutil
import tempfile
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Optional, Union

import requests

from ..state import VERSIONS, Version
from ..utils import *
from .base import Package


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
        self.binaries = unsome(binary)
        self.binary_wrapper = binary_wrapper
        self.apps = unsome(app)
        self.fonts = unsome(font)
        super().__init__(**kwargs)

    @abstractmethod
    def get_remote_version(self) -> str:
        pass

    @property
    def local_version(self) -> Optional[str]:
        try:
            return VERSIONS[self.name].version
        except KeyError:
            return None

    def local(self, *path: str) -> str:
        if self.as_global:
            return os.path.join("/usr/local", *path)
        else:
            return local(*path)

    @abstractmethod
    def binary_path(self, binary: str) -> str:
        pass

    def install_binary(self, name: str) -> None:
        link(
            self.binary_path(name),
            self.local("bin", name),
            sudo=self.as_global,
            method="binary_wrapper" if self.binary_wrapper else None,
        )

    @abstractmethod
    def app_path(self, name: str) -> str:
        pass

    def icon_directory(self) -> Optional[str]:
        return None

    def install_app(self, name: str) -> None:
        path = self.app_path(name)
        target = self.local("share", "applications", f"{name}.desktop")
        link(path, target, sudo=self.as_global)
        icons_source = self.icon_directory()
        if icons_source:
            icons_target = self.local("share", "icons")
            icon = icon_name(path)
            if icon:
                for icon_path in files_in_recursively(icons_source, f"{icon}.*"):
                    target = transplant_path(icons_source, icons_target, icon_path)
                    link(icon_path, target, sudo=self.as_global)

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
        link(source, target, sudo=self.as_global)
        if shutil.which("fc-cache"):
            run("fc-cache", "-f", font_dir)

    def install(self) -> None:
        for binary in self.binaries:
            self.install_binary(binary)
        for app in self.apps:
            self.install_app(app)
        for font in self.fonts:
            self.install_font(font)
        VERSIONS[self.name] = Version(version=self.get_remote_version())


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
        result = local(f"{self.name.replace('/', '--')}.app")
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

    @property
    def name(self):
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

    def get_remote_version(self) -> str:
        head_response = requests.head(self.url, allow_redirects=True)
        return head_response.headers["etag"]
