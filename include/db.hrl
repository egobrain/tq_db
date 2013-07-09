-record(access_mode,{
		  r = true :: boolean(),
		  sr = true :: boolean(),
		  w = true :: boolean(),
		  sw = true :: boolean()
		 }).

-record(model, {module,
				fields = [],
				table,
				stores_in_db,

				%% Generate
				get=false,
				save=false,
				delete=false,
				find=false
			   }).

-record(field, {name,
				type,
				alias,
				is_index = false,
				mode = #access_mode{}
			   }).
