import argparse
import concurrent.futures
import os
from typing import Callable

import tqdm  # type: ignore
import yaml

from .dotfiles import makelinks
from .package import Package, parse_package
from .utils import ROOT_DIR, home


def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs="*", help="The component to install")
    return parser


def load_packages(component: str) -> list[Package]:
    with open(os.path.join(ROOT_DIR, "packages", f"{component}.yaml")) as f:
        packages = yaml.safe_load(f)
        return list(map(parse_package, packages))


def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = [
        package for component in components for package in load_packages(component)
    ]
    with tqdm.tqdm(total=len(packages)) as progress:
        with concurrent.futures.ThreadPoolExecutor(20) as executor:

            def ensure(package: Package) -> None:
                package.ensure()
                progress.update(1)

            executor.map(ensure, packages)

    def dotfile(home_dir: str) -> Callable[[str], str]:
        return lambda path: os.path.join(home_dir, "." + path)

    makelinks(os.path.join(ROOT_DIR, "dotfiles"), dotfile(home()))
    makelinks(os.path.join(ROOT_DIR, "bin"), home(".local", "bin"), shallow=True)
    makelinks(os.path.join(ROOT_DIR, "config"), home(".config"), shallow=True)

    def basename_filter(*allowed: str) -> Callable[[str], bool]:
        return lambda path: os.path.basename(path) in allowed

    makelinks(
        os.path.join(ROOT_DIR, "dotfiles"),
        dotfile("/root"),
        sudo=True,
        file_filter=basename_filter("dircolors", "zshrc", "zshenv"),
    )
    makelinks(
        os.path.join(ROOT_DIR, "config"),
        "/root/.config",
        sudo=True,
        shallow=True,
        file_filter=basename_filter("bat", "nvim", "git"),
    )


main()
