# repld

Spawn an interactive `psql` repl that is also listening on a unix socket:

```
> repld serve psql
```

Send input from another process:

```
> repld send <<< 'create table foo (bar int);'
```

### vim

Example `vim` settings:

```
nn <Space>s m`vip:!repld send<CR>``
vn <Space>s :!repld send<CR>
```

In normal mode, press `Space-s` to send the inner paragraph to the `repld`
server. Or, in visual mode, press `Space-s` send the highlighted text.
