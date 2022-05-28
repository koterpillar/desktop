import configparser
import os
import shutil
from abc import ABCMeta, abstractmethod
from typing import Optional

from ..fs import files_in_recursively, link, makedirs, transplant_path
from ..state import VERSIONS, Version
from ..utils import *
from .base import Package


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
        with_os(linux=self.install_app_linux, macos=self.install_app_macos)(name)

    def install_app_linux(self, name: str) -> None:
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

    def install_app_macos(self, name: str) -> None:
        # FIXME: copy to /Applications and/or ~/Applications; ensure names,
        # etc. are correct
        pass

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
