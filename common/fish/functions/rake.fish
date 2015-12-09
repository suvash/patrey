function rake
	if test -f $PWD/Vagrantfile
		echo 'Running rake inside vagrant...'
		vagrant ssh -c "cd /vagrant; bundle exec rake $argv"
	else
		command rake $argv
	end
end
