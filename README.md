# repld

Spawn an interactive `psql` repl that is also listening on a unix socket:

```
> repld serve 'psql -U postgres'
```

Send input to it from another process:

```
> repld send <<< 'create table foo (bar int);'
```

### vim

Example `vim` settings:

```
nn <silent> <Space>s m`vip:!repld send<CR>``
nn <silent> <Space>S m`:%!repld send<CR>``
vn <silent> <Space>s :!repld send<CR>
```

In normal mode, press `Space-s` to send the inner paragraph to the `repld`
server, or `Space-S` to send the whole buffer.

In visual mode, press `Space-s` send the highlighted text.
