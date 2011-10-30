-module(quickrel).
-export([build/2]).

build(Reltool, Path0) ->
   Path = filename:absname(Path0),
   {ok, ReltoolConfig0} = file:consult(Reltool),
   {sys, RelProps} = ReltoolConfig = {sys, proplists:get_value(sys, ReltoolConfig0)},
   BootRel = proplists:get_value(boot_rel, RelProps),
   {ok, Cwd} = file:get_cwd(),
   file:set_cwd(filename:dirname(Reltool)),
   %%
   filelib:ensure_dir(filename:join([Path, "lib"]) ++ "/"),
   {ok, RelSrv} = reltool:start_server([{config, ReltoolConfig}]),
   {ok, Rel} = reltool:get_rel(RelSrv, BootRel),
   {ok, Script} = reltool:get_script(RelSrv, BootRel),
   
   {release, {BootRel, _RelVer}, _Erts, Deps} = Rel,
   file:set_cwd(Cwd),

   [ process_dep(filename:join([Path, "lib"]), Dep) || Dep <- Deps ],

   ScriptFile = filename:join([Path, BootRel ++ ".script"]),
   file:write_file(ScriptFile, io_lib:format("~p.~n",[Script])),
   systools:script2boot(filename:join([filename:dirname(ScriptFile),
         filename:basename(ScriptFile, ".script")])),

   %% 
   ok.


 %% internal

 process_dep(Path, {Name, Version}) ->
   LibDir = code:lib_dir(Name),
   ok = file:make_symlink(filename:absname(LibDir), filename:join([Path, atom_to_list(Name) ++ "-" ++ Version]));
 process_dep(Path, {Name, Version, _}) ->
   process_dep(Path, {Name, Version}).
