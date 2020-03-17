# Nut Score

I cloned most of this start boilerplate from
github.com/BuckleScript/bucklescript by running the following:

```shell
$ cd ~/programming
$ git clone git@github.com:BuckleScript/bucklescript
$ cp -R ./jscomp/bab/templates/react-hooks ~/briefcase/nut-score
```

## Installing

```shell
$ nix-shell
$ yarn install
```

## Developing

In one shell run:

```shell
$ nix-shell
$ yarn server
```

In another shell run:

```shell
$ nix-shell
$ yarn start
```

Now open a web browser and visit `localhost:8000`.
