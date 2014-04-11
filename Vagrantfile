# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

    config.vm.provider "virtualbox" do |vb|
        vb.customize ["modifyvm", :id, "--cpus", "2"]
        vb.customize ["modifyvm", :id, "--pae", "on"]
        vb.customize ["modifyvm", :id, "--memory", "2048"]
        vb.customize ["modifyvm", :id, "--acpi", "on"]
        vb.customize ["modifyvm", :id, "--hpet", "on"]
        vb.customize ["modifyvm", :id, "--hwvirtex", "on"]
    end

    config.vm.define :default, primary: true do |default|
      default.vm.box = "rinamo_base"
      default.vm.network "private_network", ip: '10.0.0.2'
      default.vm.network "forwarded_port", guest: 8000, host: 18000
      default.vm.network "forwarded_port", guest: 8098, host: 18098
      default.vm.provision 'shell', path: 'provision.sh'
    end

    # https://github.com/mitchellh/vagrant/issues/2211
    if (ARGV.include?("full") || !["ssh", "ssh-config", "up", "halt", "destroy", "provision", "reload", "suspend", "resume"].include?(ARGV.at(0)))
        config.vm.define :full do |full|
          full.vm.box = "chef/ubuntu-13.04"
          full.vm.network "private_network", ip: '10.0.0.3'
          full.vm.provision 'shell', path: 'deploy/base_build.sh'
        end
    end

end
