import argparse
import concurrent.futures

import tqdm  # type: ignore

from .dotfiles import load_links
from .package import Package, load_packages


def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs="*", help="The component to install")
    return parser


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

    links = load_links()
    for link in links:
        link.install()


main()
