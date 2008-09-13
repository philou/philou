-module(colors).
-compile(export_all).
-include("records.hrl").

scale(K, #color{r=R,g=G,b=B}) when K >= 0.0, 1.0 >= K ->
    #color{r=K*R,g=K*G,b=K*B}.
    
