{...}: {
  services.music-assistant = {
    enable = true;
    providers = [
      # Components required to complete the onboarding
      "airplay"
      "sendspin"
      "dlna"
      "chromecast"
      "heos"

      # additional
      # # "airplay"
      # "airplay_receiver"
      # "alexa"
      # "apple_music"
      # "ard_audiothek"
      # "ariacast_receiver"
      # "audible"
      # "audiobookshelf"
      # "bandcamp"
      "bbc_sounds"
      # "bluesound"
      "builtin"
      # # "chromecast"
      "coverartarchive"
      # "dashie_kiosk"
      # "deezer"
      # "digitally_incorporated"
      # # "dlna"
      # "emby"
      "fanarttv"
      "filesystem_local"
      "filesystem_nfs"
      "filesystem_smb"
      "fully_kiosk"
      "genius_lyrics"
      # "gpodder"
      "hass"
      "hass_players"
      # # "heos"
      # "ibroadcast"
      # "internet_archive"
      # "itunes_podcasts"
      # "jellyfin"
      # "kion_music"
      # "lastfm_scrobble"
      # "listenbrainz_scrobble"
      "lrclib"
      # "musicbrainz"
      # "musiccast"
      # "nicovideo"
      # "nugs"
      "opensubsonic"
      # "orf_radiothek"
      # "pandora"
      # "party"
      # "phishin"
      # "plex"
      # "plex_connect"
      # "podcast_index"
      # "podcastfeed"
      # "qobuz"
      "radiobrowser"
      "radioparadise"
      # "roku_media_assistant"
      # # "sendspin"
      # "siriusxm"
      # "snapcast"
      # "somafm"
      # "sonos"
      # "sonos_s1"
      # "soundcloud"
      # "spotify"
      # "spotify_connect"
      # "squeezelite"
      # "subsonic_scrobble"
      # "sync_group"
      # "test"
      # "theaudiodb"
      # "tidal"
      "tunein"
      # "universal_group"
      # "universal_player"
      # "vban_receiver"
      # "yandex_music"
      # "yousee"
      # "ytmusic"
      # "zvuk_music"
    ];
  };

  networking.firewall = {

    # ----------------------------------------------------
    # 1. FIXED CONTROL & WEB INTERFACE PORTS
    # ----------------------------------------------------
    allowedTCPPorts = [
      8095   # Music Assistant Web UI / Core API
      8927   # CRITICAL: Sendspin Audio WebSocket Server
      1255   # HEOS Command Line Interface (CLI)
      5000   # AirPlay 1 Connection / Control
      7000   # AirPlay 2 RTSP Connection / Control
      8008   # Chromecast Local Web API
      8009   # Chromecast Control Protocol (TLS signaling)
    ];

    # ----------------------------------------------------
    # 2. DYNAMIC MEDIA STREAMING RANGE
    # ----------------------------------------------------
    allowedTCPPortRanges = [
      { from = 8097; to = 8100; }   # Music Assistant streaming pipeline fallbacks
      { from = 49152; to = 49154; } # HEOS / DLNA UPnP Listening ports
    ];

    allowedUDPPortRanges = [
      { from = 319; to = 320; }     # AirPlay 2 PTP clock sync
      { from = 6001; to = 6015; }   # AirPlay Audio RTP stream data packets
      { from = 32768; to = 60999; } # Raw PCM/FLAC streams for Chromecast, DLNA, & AirPlay
    ];

    # ----------------------------------------------------
    # 3. MULTICAST DISCOVERY (mDNS covers Sendspin/AirPlay/Cast)
    # ----------------------------------------------------
    allowedUDPPorts = [
      1900   # SSDP (DLNA / UPnP / HEOS Discovery)
      5353   # mDNS (Sendspin, Chromecast, AirPlay, Avahi)
    ];
  };


  services.cloudflared = {
    tunnels."lle-errbrr-com".ingress = {
      "ma.errbrr.com" = "http://localhost:${toString 8095}";
    };
  };
}
