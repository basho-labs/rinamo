# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
    config.vm.box = "chef/centos-6.5"

    config.vm.network "forwarded_port", guest: 8000, host: 18000
    config.vm.network "forwarded_port", guest: 8098, host: 18098

    # config.vm.synced_folder "../data", "/vagrant_data"

    config.vm.provider "virtualbox" do |vb|
        vb.customize ["modifyvm", :id, "--cpus", "2"]
        vb.customize ["modifyvm", :id, "--memory", "2048"]
        vb.customize ["modifyvm", :id, "--acpi", "on"]
        vb.customize ["modifyvm", :id, "--ioacpi", "on"]
        vb.customize ["modifyvm", :id, "--hpet", "on"]
        vb.customize ["modifyvm", :id, "--hwvirtex", "on"]
    end

    config.vm.provision 'shell', path: 'provision.sh'
end
