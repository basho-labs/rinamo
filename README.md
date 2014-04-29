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

**Note**: If you're using Vagrant 1.5+, use the following command to install
`vagrant-berkshelf`:

```bash
$ vagrant plugin install vagrant-berkshelf --plugin-version 2.0.0.rc3
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

Once running, Rinamo can be updated at any time by rebuilding the Riak
release by running the provision command:

```bash
$ vagrant provision rinamo-dev
```

## Rinamo Client Test Console

Rinamo responds to requests issued by AWS Client APIs.  Anything can be used, but we package up a scala test console for use during development.  To fire it up run the following from within the vagrant development environment:

```bash
$ /vagrant/tests/com.basho.dynamodb.integ
$ mvn test-compile scala:console; reset
```
Then run commands like this:

```scala
scala> Table.create("books_range", "Id", "N", Some("Title"), Some("S"))
res0: com.amazonaws.services.dynamodbv2.model.CreateTableResult = { ...

scala> Table.list()
res1: List[String] = List(books_range)
```

For more examples, refer to [the console examples](https://github.com/basho-labs/rinamo/tree/rs-indexing/tests/com.basho.dynamodb.integ/console).

## Additional documentation

- [NOTES.md](src/NOTES.md)
- [TODO.md](src/TODO.md)
