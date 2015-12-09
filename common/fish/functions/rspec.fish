function rspec
	if test -f $PWD/Vagrantfile
		echo 'Running rspec inside vagrant...'
		vagrant ssh -c "cd /vagrant; bundle exec rspec $argv"
	else
		command rspec $argv
	end
end
