scylla-tui
==========

Scylla terminal UI

Usage
-----


```bash
# provide config
# copy and edit provided sample
cp scylla-api.conf ~/.scylla-api.conf

nix build
./result/bin/scylla-tui

# or just
SCYLLA_API_CONF=./scylla-api.conf ./result/bin/scylla-tui
```

Configuration
-------------

API tries to load config from `~/.scylla-api.conf` or according to `SCYLLA_API_CONF` if provided.
