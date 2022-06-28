# https://stackoverflow.com/a/59942079
run:
	dotnet run --project src/FSharpApp/FSharpApp.fsproj $(filter-out $@,$(MAKECMDGOALS))

%:      # thanks to chakrit
    @:    # thanks to William Pursell