# Rinamo

A DynamoDB API implementation built on Riak 2.0 + Search (Yokozuna/Solr).

## Building

```bash
$ make
```

## Testing

```bash
$ make test
```

## Development environment

A virtual development environment for Rinamo can be built using the steps
below.

### Install Vagrant

Download and install Vagrant via the
[Vagrant installer](http://downloads.vagrantup.com/).

### Install Vagrant plugins

``` bash
$ vagrant plugin install vagrant-berkshelf
$ vagrant plugin install vagrant-omnibus
$ vagrant plugin install vagrant-cachier (Use RINAMO_USE_CACHE to enable)
```

### Add your local SSH key to `ssh-agent`

The `Vagrantfile` is configured to enable agent forwarding over SSH, but your
local key must be added to `ssh-agent`:

```bash
$ ssh-add .ssh/id_rsa
```

### Create base development environment

The development environment for Rinamo has several dependencies that take a
while to build. In order to expedite the process, the base development
environment can be built once, and made into a Vagrant base box:

```bash
$ RINAMO_USE_CACHE=1 vagrant up rinamo-base
$ vagrant package rinamo-base
$ vagrant box add rinamo-base package.box
$ rm package.box
```

### Provision Rinamo development environment

Now that the `rinamo-base` box is built, Rinamo itself can be provisioned:

```bash
$ vagrant up rinamo-dev
```

## Additional documentation

- [NOTES.md](src/NOTES.md)
- [TODO.md](src/TODO.md)
