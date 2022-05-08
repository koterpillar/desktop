import os
from typing import Callable, Union

import yaml

from .utils import *


class Links:
    def __init__(
        self,
        source: str,
        dest: str,
        dot: bool = False,
        sudo: bool = False,
        shallow: bool = False,
        only: Some[str] = None,
    ) -> None:
        self.source = os.path.join(ROOT_DIR, source)
        if not dest.startswith("/"):
            dest = os.path.join(home(), dest)
        self.dest = dest
        self.dot = dot
        self.sudo = sudo
        self.shallow = shallow
        self.only = unsome_(only)

    def install(self) -> None:
        paths: Iterator[str]
        if self.shallow:
            paths = map(lambda entry: entry.path, os.scandir(self.source))
        else:
            paths = filter(os.path.isfile, files_in_recursively(self.source))

        for path in paths:
            if self.only and os.path.basename(path) not in self.only:
                continue

            target = os.path.relpath(path, self.source)
            if self.dot:
                target = "." + target
            target = os.path.join(self.dest, target)

            print(f"{path=} {target=}")
            link(path, target, sudo=self.sudo)


def load_links() -> list[Links]:
    with open(os.path.join(ROOT_DIR, "links.yaml")) as f:
        links = yaml.safe_load(f)
        return list(Links(**args) for args in links)
