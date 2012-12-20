%% @author ivershinina
%% @doc @todo Add description to records.

-record(data, {key, data}).

-record(circle, {radius = 0}).
-record(rectangle, {length = 0, 
                    width = 0}).
-record(triangle, {a = 0, 
                   b = 0, 
                   c = 0}).

-record(binary_tree, {value = 0,
                      left_child = 0,
                      right_child = 0}).
-record(leaf, {value = 0}).