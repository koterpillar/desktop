import os
import tempfile
from abc import ABCMeta, abstractmethod
from typing import Optional, Union

from ..fs import make_executable, makedirs
from ..utils import *
from .manual import ManualPackage


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
