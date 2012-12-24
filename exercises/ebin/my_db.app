{application, my_db,
 [{description, "Database"},
  {vsn, "1.0"},
  {modules, [my_db_gen, my_db_supervisor, my_db_app]},
  {registered, [my_db_gen, my_db_sup]},
  {applications, [kernel, stdlib]},
  {mod, {my_db_app, []}}]}.