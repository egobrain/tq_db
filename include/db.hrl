-record(access_mode,{
		  r = true :: boolean(),
		  sr = true :: boolean(),
		  w = true :: boolean(),
		  sw = true :: boolean()
		 }).

-record(model, {module,
				fields = [],
				table,
				stores_in_db}).

-record(field, {name,
				type,
				alias,
				mode = #access_mode{}
			   }).
