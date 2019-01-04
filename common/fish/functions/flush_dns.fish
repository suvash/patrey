function flush_dns
  echo "Cleaning DNS cache..."
  switch (uname)
  case Darwin
    sudo killall -HUP mDNSResponder;sudo killall mDNSResponderHelper;sudo dscacheutil -flushcache
  case '*'
    echo "DNS flush not setup for this OS."
  end

end
