import os
from typing import Callable, Union

from .utils import *


def makelinks(
    source: str,
    dest: Union[str, Callable[[str], str]],
    *,
    sudo: bool = False,
    shallow: bool = False,
) -> None:
    paths: Iterator[str]
    if shallow:
        paths = map(lambda entry: entry.path, os.scandir(source))
    else:
        paths = filter(os.path.isfile, files_in_recursively(source))
    for path in paths:
        target = transplant_path(source, dest, path)
        link(path, target, sudo=sudo)
