-module(create_tables).

-export([init_tables/0, insert_user/3, insert_project/2]).

-include("create_tables.hrl").

init_tables() ->
  mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
  mnesia:create_table(project, [{attributes, record_info(fields, project)}]),
  mnesia:create_table(contributor, [{type, bag}, {attributes, record_info(fields, contributor)}]).

insert_project(Title, Description) ->
  mnesia:dirty_write(#project{title = Title, description = Description}).

insert_user(Id, Name, ProjectTitles) when ProjectTitles =/= [] ->
  User = #user{id = Id, name = Name},
  Fun =
    fun() ->
       % insert user
       mnesia:write(User),
       lists:foreach(fun(Title) ->
                        % make sure project exists
                        [#project{title = Title}] = mnesia:read(project, Title),
                        % connect user and project (m:m relationship)
                        mnesia:write(#contributor{user_id = Id, project_title = Title})
                     end,
                     ProjectTitles)
    end,
  mnesia:transaction(Fun).
