# repld

Spawn an interactive `psql` repl that is also listening on a unix socket:

```
> repld serve 'psql -U postgres'
```

Send input to it from another shell:

```
> repld send <<< 'create table foo (bar int);'
```

If you want to see the input you send via `repl send` echoed, run `repld serve`
with `--echo`.

### vim

Example `vim` settings:

```
nn <silent> <Space>s m`vip:<Left><Left><Left><Left><Left>silent <Right><Right><Right><Right><Right>w !repld send<CR>``
nn <silent> <Space>S m`:silent w !repld send<CR>``
vn <silent> <Space>s m`:<Left><Left><Left><Left><Left>silent <Right><Right><Right><Right><Right>w !repld send<CR>``
```

In normal mode, press `Space-s` to send the inner paragraph to the `repld`
server, or `Space-S` to send the whole buffer.

In visual mode, press `Space-s` send the highlighted lines.
