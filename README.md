virtuerl
=====

A minimalistic VM manager.

# Required packages
```sh
sudo apt install ovmf swtpm
```

# Windows Guests

## Disable boot prompt
```sh
sudo mount -o loop Win11_23H2_EnglishInternational_x64v2.iso /mnt/win

genisoimage \
    --allow-limited-size \
    -no-emul-boot \
    -b "boot/etfsboot.com" \
    -boot-load-seg 0 \
    -boot-load-size 8 \
    -eltorito-alt-boot \
    -no-emul-boot \
    -e "efi/microsoft/boot/efisys_noprompt.bin" \
    -boot-load-size 1 \
    -iso-level 4 \
    -udf \
    -o "win.iso" \
    /mnt/win/
```

# Running

On the server

Make sure IP forwarding is enabled (`/etc/sysctl.conf`)
```
# Uncomment the next line to enable packet forwarding for IPv4
net.ipv4.ip_forward=1
# Uncomment the next line to enable packet forwarding for IPv6
net.ipv6.conf.all.forwarding=1
```
Run `sysctl -w` to commit changes.

```sh
sudo -s ./erts-13.1.5/bin/erl -mode embedded -boot releases/0.0.0/start -config releases/0.0.0/sys.config -proto_dist inet6_tcp -name virtuerl@myserver.com -setcookie abcdef
```

Locally
```sh
rebar3 compile
erl -name moi -proto_dist inet6_tcp -setcookie abcdef -pa _build/default/lib/*/ebin -hidden
(moi@t460s.lan)1> net_adm:ping('virtuerl@myserver.com').
pong
(moi@t460s.lan)2> virtuerl_ui:start('virtuerl@myserver.com').
```
