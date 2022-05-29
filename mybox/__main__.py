import argparse

from .dotfiles import load_links
from .package import load_packages
from .utils import flatten, parallel_map_tqdm


def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs="*", help="The component to install")
    return parser


def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = flatten(map(load_packages, components))
    parallel_map_tqdm(packages, lambda p: p.ensure())
    links = load_links()
    for link in links:
        link.install()


main()
