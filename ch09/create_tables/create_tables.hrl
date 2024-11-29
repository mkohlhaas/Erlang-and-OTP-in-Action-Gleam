-record(user, {id, name}).
-record(project, {title, description}).
% intersection table
-record(contributor, {user_id, project_title}).
