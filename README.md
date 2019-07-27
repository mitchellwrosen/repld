# repld

Spawn an foregrounded `psql` repl that is also serving the repl on a unix domain
socket:

```
> repld 'psql -U postgres'
```

Send input to it from another shell:

```
> repld-send <<< 'create table foo (bar int);'
```

### vim

Example `vim` settings:

```
nn <silent> <Space>s m`vip<Esc>:silent '<,'>w !repld-send<CR>``
nn <silent> <Space>S m`:silent w !repld-send<CR>``
vn <silent> <Space>s m`<Esc>:silent '<,'>w !repld-send<CR>``
```

In normal mode, press `Space-s` to send the inner paragraph to the `repld`
server, or `Space-S` to send the whole buffer.

In visual mode, press `Space-s` send the highlighted lines.
