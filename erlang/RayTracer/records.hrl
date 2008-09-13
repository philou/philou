-define(EPSILON,1.0e-10).

%% size
-record(size, {width=160,height=100}).
-define(SMALL,#size{width=160,height=100}).
-define(MEDIUM,#size{width=320,height=200}).
-define(LARGE,#size{width=640,height=400}).
-define(XLARGE,#size{width=1280,height=800}).

%% ppm pictures
-define(MAX_COLOR, 255).
-record(ppm_data, {width=0,height=0,max_color=?MAX_COLOR,pixels=[]}).

%% colors
-record(color, {r=0.0,g=0.0,b=0.0}).

-define(WHITE,#color{r=1.0,g=1.0,b=1.0}).
-define(BLACK,#color{r=0.0,g=0.0,b=0.0}).
-define(RED,#color{r=1.0,g=0.0,b=0.0}).
-define(BLUE,#color{r=0.0,g=0.0,b=1.0}).
-define(GREEN,#color{r=0.0,g=1.0,b=0.0}).
-define(CYAN,#color{r=0.0,g=1.0,b=1.0}).
-define(YELLOW,#color{r=1.0,g=1.0,b=0.0}).
-define(MAGENTA,#color{r=1.0,g=0.0,b=1.0}).

%% coordonates
-record(coord, {x=0.0,y=0.0,z=0.0}).
-define(ORIGIN, #coord{x=0.0,y=0.0,z=0.0}).

%% vector
-record(vector, {x=0.0,y=0.0,z=0.0}).
-define(NULL_VECTOR, #coord{x=0.0,y=0.0,z=0.0}).

%% ray
-record(ray, {origin=?ORIGIN, direction=#vector{z=1.0}}).

%% texture
-record(texture, {color=?RED, ambient=0.0}).

%% spheres
-record(sphere, {center=?ORIGIN, radius=1,texture=#texture{}}).

%% scene
-record(scene, {background=?WHITE, objects=[],lights=[]}).
