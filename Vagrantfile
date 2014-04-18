# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |cluster|
  # Ensure latest version of Chef is installed
  cluster.omnibus.chef_version = :latest

  # Utilize the Cachier plugin to cache downloaded packages
  if Vagrant.has_plugin?("vagrant-cachier") && !ENV["RINAMO_USE_CACHE"].nil?
    cluster.cache.scope = :box
  end

  # Utilize the Berkshelf plugin to resolve cookbook dependencies
  cluster.berkshelf.enabled = true

  cluster.vm.define "rinamo-base".to_sym do |config|
    config.vm.box = "ubuntu/trusty64"
    config.vm.provider "virtualbox" do |vb|
        vb.customize ["modifyvm", :id, "--cpus", "2"]
        vb.customize ["modifyvm", :id, "--pae", "on"]
        vb.customize ["modifyvm", :id, "--memory", "2048"]
        vb.customize ["modifyvm", :id, "--acpi", "on"]
        vb.customize ["modifyvm", :id, "--hpet", "on"]
        vb.customize ["modifyvm", :id, "--hwvirtex", "on"]
    end

    # Network
    config.vm.hostname = "rinamo-base"
    config.vm.network :private_network, ip: "10.0.0.2"

    # SSH agent forwarding
    config.ssh.forward_agent = true

    # Provision using Chef
    config.vm.provision :chef_solo do |chef|
      chef.roles_path = "deploy/roles"
      chef.add_role "base"

      chef.json = { }
    end
  end

  cluster.vm.define "rinamo-dev".to_sym do |config|
    config.vm.box = "rinamo-base"
    config.vm.provider "virtualbox" do |vb|
        vb.customize ["modifyvm", :id, "--cpus", "2"]
        vb.customize ["modifyvm", :id, "--pae", "on"]
        vb.customize ["modifyvm", :id, "--memory", "2048"]
        vb.customize ["modifyvm", :id, "--acpi", "on"]
        vb.customize ["modifyvm", :id, "--hpet", "on"]
        vb.customize ["modifyvm", :id, "--hwvirtex", "on"]
    end

    # Network
    config.vm.hostname = "rinamo"
    config.vm.network :private_network, ip: "10.0.0.2"

    # SSH agent forwarding
    config.ssh.forward_agent = true

    config.vm.provision "shell", path: "deploy/provision.sh"
  end
end
