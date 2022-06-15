# https://stackoverflow.com/a/59942079
build:
	dotnet build
clean:
	dotnet clean
restore:
	dotnet restore
test: 
	dotnet test
watch:
	dotnet watch --project src/FSharpApp/FSharpApp.fsproj run
start:
	dotnet run --project src/FSharpApp/FSharpApp.fsproj