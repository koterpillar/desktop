import subprocess
import sys
from typing import Literal, Optional, TypeVar, Union, cast

OS = Literal["linux", "darwin"]

CURRENT_OS: OS = cast(OS, sys.platform)

T = TypeVar("T")


def with_os(*, linux: T, macos: T) -> T:
    if CURRENT_OS == "linux":
        return linux
    elif CURRENT_OS == "darwin":
        return macos
    else:
        raise ValueError(f"Unexpected OS {CURRENT_OS}.")


Some = Optional[Union[T, list[T]]]


def unsome(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


def run(*args, **kwargs) -> subprocess.CompletedProcess:
    return subprocess.run(args, check=True, **kwargs)
