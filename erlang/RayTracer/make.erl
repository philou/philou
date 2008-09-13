-module(make).
-compile(export_all).

doc() ->
    edoc:files(["colors.erl", "image.erl", "vectors.erl", "polynoms.erl", "raytracer_impl.erl", "raytracer.erl"]).
