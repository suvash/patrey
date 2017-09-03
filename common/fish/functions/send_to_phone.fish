function send_to_phone
	curl -s \
  -F "token=$PUSHOVER_APPLICATION_TOKEN" \
  -F "user=$PUSHOVER_USER_TOKEN" \
  -F "message=$argv" \
  -F "device=$PUSHOVER_DEVICE_NAME" \
  https://api.pushover.net/1/messages.json
end
