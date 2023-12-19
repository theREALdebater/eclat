-----------------------------------------------------------------------------------------------
--/ Titian.Components package specification:

--| Copyright (C) 2012 Nicholas James Roberts (South Croydon, Surrey, UK).

--| Part of the Titian Graphics Library. See the bottom (end) of this file for important legal
--| information.

-----------------------------------------------------------------------------------------------
with Ada.Finalization, Ada.Strings.Wide_Unbounded;
with Titian.Raster_IO.Interactive, Titian.Text.Formatting;

use Ada.Finalization, Ada.Strings.Wide_Unbounded;
use Titian.Raster_IO.Interactive;

-----------------------------------------------------------------------------------------------
package Titian.Components is

   pragma Pure;


   --------------------------------------------------------------------------------------------
   --/ Root component type:

   type Root_Component_Type is abstract new Limited_Controlled with private;

   --| A 'component' is a graphical item which is displayed on an interactive graphical
   --| device as a part of a graphical user interface (GUI), and which embodies one notional
   --| element of the interface, from the user's point of view.

   --| All component types are derived from the abstract type `Root_Component_Type`. Typical
   --| components will include: text boxes; pictures; command buttons (push-buttons); choice
   --| (radio) buttons; toggle buttons (check boxes); scrollable panels; splitter panels; many
   --| others.

   --\
   --------------------------------------------------------------------------------------------
   --/ Extent of a component:

   function Extent (Component: in Root_Component_Type) return Orthorect;

   function Set_Extent (Component: in Root_Component_Type;
                        Extent:    in Orthorect);

   --| Every component has an 'extent' property. The extent of a component is the orthogonal
   --| rectangle within which the component is to draw its content, whatever that may be.

   --| Whenever the component's container calls upon a component to redraw itself (by calling
   --| its `Draw` procedure), it will first set the origin of the context to correspond to
   --| the origin of the component's extent, if there is a difference.

   --| A container can call a component's `Recommended_Extent` function to help it decide
   --| what extent to impose upon the component. Typically, a container will choose an extent
   --| that causes the component's origin the coincide with the container's (so that changing
   --| the origin is unnecessary when redrawing).

   --| The default implementation of `Set_Extent` simply remembers the specified extent, and
   --| the default implementation of `Extent` returns this value. The initial value is
   --| ((0,0),(-1,-1)).

   --\
   --------------------------------------------------------------------------------------------
   --/ Context of a component:

   type Component_Context_Access is access all Interactive_Context'Class;

   function Context (Component: in Root_Component_Type) return Component_Context_Access;

   procedure Set_Context (Component: in out Root_Component_Type;
                          Context:   in     Component_Context_Access);

   --| Every component has a 'context' property, which is the interactive graphical context
   --| on which the component is (currently) displayed.

   --| The default implementation of `Set_Context` simply remembers the given access value,
   --| and the default implementation of `Context` returns this value. The value may be null,
   --| which is the initial value.

   --\
   --------------------------------------------------------------------------------------------
   --/ Default color of a component:

   function Default_Color (Component: in out Root_Component_Type) return Basic_Color;

   procedure Set_Default_Color (Component: in out Root_Component_Type;
                                Color:     in     Basic_Color);

   --| Every component has a 'default color' property, which is used by the default
   --| implementation of the `Draw` procedure (below).

   --| The default implementation of `Set_Default_Color` simply remembers the specified
   --| color, and the default implementation of `Default_Color` returns this value. The
   --| initial value is implementation defined.

   --\
   --------------------------------------------------------------------------------------------
   --/ Drawing a component:

   procedure Draw (Component: in out Root_Component_Type;
                   Regions:   in     Orthorect_Set);

   --| Whenever the container of a component wishes the component to draw parts (or all) of
   --| itself, it sets the origin (as described above) and calls the component's `Draw`
   --| procedure, with the `Regions` parameter indicating the parts to be drawn.

   --| It is conventional etiquette that the container never calls `Draw` with the
   --| `Regions` parameter null (but implementations would be prudent to handle this case
   --| gracefully anyway). The container will normally set the clipping of the context to the
   --| component's extent. In a secure environment this clipping may be enforced.

   --| There are no timing or synchronisation limitations to the time at which the `Draw`
   --| procedure may be called. In particular, it is possible that it could be called in
   --| parallel with calls to any of the component's other procedures, and it is even possible
   --| that `Draw` could be called several times in parallel with itself (for different
   --| components of the same type). For this reason, it is necessary that the component
   --| implements all the necessary synchronisation of access to any shared variables (for
   --| example by the use of a protected object).

   --| The default implementation of `Draw` simply fills each rectangle in `Regions` with
   --| the component's default color (which it obtains by calling the `Default_Color`
   --| function).

   --\
   --------------------------------------------------------------------------------------------
   --/ Component refreshing:

   function Needs_Drawing (Component: in Root_Component_Type) return Orthorect_Set;

   --| The `Needs_Redrawing` function indicates the parts of a component that need to be
   --| redrawn. This will usually be because one or more of whatever the values of the
   --| conceptual entity to which the component corresponds have changed since the last time
   --| they were (fully) drawn. Thus the `Draw` procedure (above) may need to record which
   --| parts have been redrawn.

   --| It is usually pointless for other procedures of the component to do any drawing
   --| themselves. Instead, they should do whatever is necessary to cause the `Needs_Drawing`
   --| function to (additionally) return the relevant region of the component. In this way,
   --| that region will be redrawn at an appropriate time (as decided by the component's
   --| container, often itself under the direction of other software).

   --| The default implementation of this function simply calls the `Extent` function (see
   --| above), and returns a singleton set built from the result. Generally, it will be good
   --| practice for a component to return a reasonable minimum region that truly needs to be
   --| redrawn; in particular, this function should return a null set if none of the component
   --| needs redrawing. The continual redrawing of an entire component is likely to lead to
   --| significant inefficiency (except perhaps for very small or very dynamic components) in a
   --| typical GUI system.

   --\
   --------------------------------------------------------------------------------------------
   --/ Recommended extent and panning steps:

   function Recommended_Width (Component:        in Root_Component_Type;
                               Suggested_Width:  in Absolute_Pixels;
                               Suggested_Height: in Absolute_Pixels) return Absolute_Pixels;

   function Recommended_Height (Component:        in Root_Component_Type;
                                Suggested_Width:  in Absolute_Pixels;
                                Suggested_Height: in Absolute_Pixels) return Absolute_Pixels;

   --| When a container needs to know what width or height to use for the extent of a
   --| component, it can call either of these two functions. The `Suggested_Width` and
   --| `Suggested_Height` parameters are for the component to use in guiding its
   --| recommendation.

   --| The default implementation of `Recommended_Width` simply returns the value of the
   --| `Suggested_Width` parameter, and that of `Recommended_Height` simply returns the
   --| value of the `Suggested_Height` parameter.

   type Panning_Direction is (Left, Right, Up, Down);

   type Panning_Step_Size is (Small, Large);

   function Recommended_Pan  (Component: in Root_Component_Type;
                              Direction: in Panning_Direction;
                              Size:      in Panning_Step_Size := Small) return Absolute_Pixels;

   --| When a request is made to a viewport to pan (scroll) the viewable rectangle one logical
   --| step left, right, up, or down, the viewport can call the `Recommended_Pan` function to
   --| obtain from its canvas (a suggestion as to) the appropriate number of pixels to pan. A
   --| small step is e.g. one text line and a large step is e.g. one visible page.

   --| The default implementation of this function returns an implementation-defined constant.
   --| For a small step this is typically 1, and for a large step it is typically 50.

   --\
   --------------------------------------------------------------------------------------------
   --/ :

   --\
   --------------------------------------------------------------------------------------------
   --/ :

   type Control_Component is abstract new Root_Component_Type with private;

   --| A 'control component' is a component which either displays one particular aspect of
   --| the program's notional model to the user, or which provides a way for the user to
   --| manipulate an aspect of the program's notional model. Examples include: a progress bar,
   --| which visually fills up in order to display the relative progress of a particular job; a
   --| slide bar, which visually emulates a linear potentiometer and allows the user to
   --| progressively adjust a linear variable of the program's notional model (ideally causing
   --| continuous visual feedback of its effect).

   --\
   --------------------------------------------------------------------------------------------
   --/ Control component activation and deactivation:

   type Activation_State is (Deactivated, Activated);

   function Activation (Component: in Control_Component) return Activation_State;

   procedure Set_Activation (Component: in out Control_Component;
                             State:     in     Activation_State);

   --| A control component has an 'activation state' property, which has two values:
   --| 'activated' and 'deactivated'. When a component is activated, it actively displays
   --| the aspect of the program's notional model that it is supposed to, generally including
   --| continuous updates if appropriate, but at the least updating its display whenever a user
   --| action causes a change to that aspect of the model. When it is deactivated, it should
   --| redisplay itself in a way which clearly shows it is not in an active state, and
   --| generally does not change this display until it becomes activated again (but `Draw`
   --| must still redraw it as necessary).

   --| The default implementation of `Set_Activation` procedure simply remembers the state
   --| specified, and the default implementation of `Activation` returns this value. Every
   --| control component is initially deactivated.

   --\
   --------------------------------------------------------------------------------------------
   --/ Hints and queries:

--   function Hint (Component: in Control_Component) return Universal_String;
--
--   procedure Set_Hint (Component: in out Control_Component;
--                       Hint:    in     Universal_String);
--
--   function Query (Component: in Control_Component) return Help_Query_Access;
--
--   procedure Set_Query (Component: in out Control_Component;
--                        Query:   in     Help_Query_Access);
--
--   --| A control component ...
--
--   --| The default implementation of the ...

   --\
   --------------------------------------------------------------------------------------------
   --/ :

   --\
   --------------------------------------------------------------------------------------------
   --/ Interactive component type:

   type Interactive_Component is abstract new Control_Component with private;

   --| An 'interactive component' is a control component that provides some user input
   --| functionality (rather than being purely for user output). The input may be by means of
   --| keyboard, or mouse, or any other input mechanism.

   --| Guidelines on good GUI design recommend that multiple methods of input are supported
   --| where feasible. For example, an interactive component that allows the mouse to adjust a
   --| setting should also make it possible for the keyboard to be used instead, in case a user
   --| cannot use the mouse (effectively) for any reason.

   --\
   --------------------------------------------------------------------------------------------
   --/ Focus:

   function Has_Focus (Component: in Interactive_Component) return Boolean;

   procedure Set_Focus (Component: in out Interactive_Component;
                        Has_Focus: in     Boolean := True);

   --| An interactive component has a 'focus state' property, which can have two values: a
   --| component either 'has the focus' or it does not have the focus.

   --| At any one time, only one component anywhere on a single workstation can have the focus;
   --| all others will not have the focus. It is possible sometimes for no component at all to
   --| have the focus (but usually one will). User input associated with the workstation (e.g.
   --| from the keyboard, or mouse) will be directed to the component which has the focus.

   --| The `Has_Focus` function returns the focus state of an interactive component; it
   --| returns `True` if the component has the focus, or `False` otherwise. The
   --| `Set_Focus` procedure sets the focus state of an interactive component; the component
   --| will have the focus if the `Has_Focus` parameter is `True`.

   --| The default implementation of `Set_Focus` simply remembers the specified state; the
   --| default implementation of `Has_Focus` returns this value.

   --| An interactive component should indicate that it has the focus by displaying itself
   --| differently in some distinctive manner. Typically, a component's container will manage
   --| focus movement from one component `A` to another `B`, by calling
   --| `Set_Focus(A,False)` followed by `Set_Focus(B,True)`.

   --\
   --------------------------------------------------------------------------------------------
   --/ Handling keyboard and mouse events:

   procedure Handle_Event (Component: in out Interactive_Component;
                           Event:     in     Root_Event_Type'Class);

   --| In general, an interactive component can receive events. These events are usually
   --| related to user input (e.g. a mouse button or keyboard key being pressed or released, or
   --| the mouse being moved) being directed to the component which currently has the focus.

   --| The `Handle_Event` procedure is called whenever the component's container receives an
   --| event and determines that the event belongs to the component. Generally the coordinates
   --| of the point of a mouse event will be adjusted by the container to be relative to (and
   --| usually within) the extent of the component. Otherwise an event is generally unmodified.
   --| (The container may filter out certain events, typically because it handles that event
   --| itself.)

   --| The default implementation of this procedure is implementation defined, but essentially
   --| does nothing. However, any type which overrides this procedure should (in general)
   --| include in the body of the overriding procedure a call to the corresponding procedure of
   --| its parent type.



--   Normal_Functionality: constant Keyboard_Functionality := Raster_IO.Interactive.Normal_Functionality;
--   -- ( Interpretations | Autorepetition => True, Actual_Key_Events => False );
--
--   function Keyboard_Functionality (Component: in Interactive_Component) return Keyboard_Functionality;
--
--   procedure Set_Keyboard_Functionality (Component:     in out Interactive_Component;
--                                         Functionality: in     Keyboard_Functionality);




   --\
   --------------------------------------------------------------------------------------------
   --/ :

   --\
   --------------------------------------------------------------------------------------------
   --/ Viewports:

   type Viewport_Component is abstract new Interactive_Component with private;

   --| A 'viewport' is an interactive component which (generally) displays a portion of
   --| another component, which is called the viewport's 'canvas'. For example a viewport
   --| might display a part of a large image, and permit other parts of the image to be panned
   --| (scrolled) into view. Examples of viewports include: scrollable panels; ???.

   --\
   --------------------------------------------------------------------------------------------
   --/ The canvas of a viewport:

   type Component_Access is access all Root_Component_Type'Class;

   function Canvas (Viewport: in Viewport_Component) return Component_Access;

   procedure Set_Canvas (Viewport: in out Viewport_Component;
                         Canvas:   in     Component_Access);

   --| The default implementation of `Set_Canvas` simply remmembers the specified canvas
   --| access value; the default implementation of `Canvas` returns this value. The initial
   --| value is null.

   --\
   --------------------------------------------------------------------------------------------
   --/ The viewable rectangle of a viewport:

   function Viewable (Viewport: in Viewport_Component) return Orthorect is abstract;

   procedure Set_Viewable (Viewport:  in out Viewport_Component;
                           Point:     in     Raster_Point;
                           Principal: in     Principal_Point := Mid_Center) is abstract;

   --| The 'viewable rectangle' of a viewport is the part of the extent of the canvas which
   --| the viewport currently displays.

   --| The `Viewable` function returns the current viewable rectangle of a viewport.

   --| The `Set_Viewable` procedure pans (scrolls) the viewable rectangle of a viewport such
   --| that the specified `Principal` point of the viewable rectangle coincides with the
   --| specified `Point` within the extent of the canvas.

   --| If any part of the viewable rectangle lies outside the canvas' extent, that part will be
   --| redrawn filled in the canvas' background color.

   --| Whilst the user is panning (scrolling) a viewport, one or many calls will be made to
   --| `Set_Viewable`. It is permitted for the procedure to adjust the movement specified to
   --| accord with the logic of what is being displayed. For example, if text is being
   --| displayed, the movement may be rounded up or down to the nearest complete line or column
   --| of text. Therefore, a container calling this procedure should subsequently call the
   --| `Viewable` function of the viewport if it needs to ascertain the movement actually
   --| made.

   --\
   --------------------------------------------------------------------------------------------
   --/ Position of a viewport's canvas:

   function Canvas_Position (Viewport: in out Viewport_Component) return Orthorect is abstract;

   --| The `Canvas_Position` function returns the orthogonal rectangle where the viewable
   --| rectangle of the canvas is placed within the extent of a viewport.

   --\
   --------------------------------------------------------------------------------------------
   --/ :

   --\
   --------------------------------------------------------------------------------------------
   --/ Interactive container:

   type Interactive_Container is abstract new Interactive_Component with private;

   --| An 'interactive container' is an interactive component whose main (or only) purpose is
   --| to be a container for one or (more usually) several other (usually interactive)
   --| components, which are called its 'parts'. Typically an interactive container provides
   --| a convenient way to show and control a group of logically related components, so as to
   --| provide visual and operational coherency for the user. Examples of interactive
   --| containers include: splitter panels; ???.

   --\
   --------------------------------------------------------------------------------------------
   --/ Part iteration:

   function Part_Count (Container: in Interactive_Container) return Natural;

   function Part (Container: in Interactive_Container;
                  Index:     in Positive) return Component_Access;

   --| Each part of an interactive container has a 'part index', which is an integer. The
   --| first part (in the focus order) has index number 1, the following part has index 2, and
   --| so on. If the last part has index n, the 'part count' of the container is n.

   --| The `Part_Count` function returns the part count of a container. The `Part` function
   --| returns an access value referencing a part by index. These functions make all the parts
   --| of a container directly accessible.

   --| Any subprogram which specifies a part index that is out of range (its value is greater
   --| than the part count) causes `Constraint_Error` to be propagated (and otherwise does
   --| nothing).

   --| The default implementations ...

   --\
   --------------------------------------------------------------------------------------------
   --/ Part position:

   function Part_Position (Container: in Interactive_Container;
                           Index:     in Positive) return Orthorect;

   --| The `Part_Position` function returns the extent of an interactive container's part
   --| within the extent of the container. The size (width and height) of the orthogonal
   --| rectangle returned by this function will (must) always be equal to the size of the
   --| part's extent (but it may be offset from it).

   --| The default implementations ...

   --\
   --------------------------------------------------------------------------------------------
   --/ Internal focus changing:

   Focus_Error: exception;

--?   function Can_Move_Focus_Next (Container: in Interactive_Container) return Boolean is abstract;
--?   function Can_Move_Focus_Back (Container: in Interactive_Container) return Boolean is abstract;

--?   procedure Move_Focus_Next (Container: in out Interactive_Container) is abstract;
--?   procedure Move_Focus_Back (Container: in out Interactive_Container) is abstract;

   --| There is a specific order for the parts of a container, which is called the 'focus
   --| order'.

   --| The `Move_Focus_Next` procedure moves the focus from the part of the specified
   --| `Container` which currently has the focus to the following part (in the focus order).
   --| The `Move_Focus_Back` procedure moves the focus from the part of the specified
   --| `Container` which currently has the focus to the preceding part (in the focus order).

   --| The `Can_Move_Focus_Next` function returns `True` if calling `Move_Focus_Next`
   --| would be invalid (for example because the last part in the focus order has the focus).
   --| Calling `Move_Focus_Next` when this function would return `False` causes the
   --| `Focus_Error` exception to be raised. The `Can_Move_Focus_Back` function returns
   --| `True` if calling `Move_Focus_Back` would be invalid (for example because the first
   --| part in the focus order has the focus). Calling `Move_Focus_Back` when this function
   --| would return `False` causes the `Focus_Error` exception to be raised.

   function Part_Having_Focus (Container: in Interactive_Container) return Natural;

   procedure Move_Focus (Container: in out Interactive_Container;
                         Part:      in     Positive);

   --| The `Part_Having_Focus` function returns the index of the part of an iteractive
   --| container that currently has the focus. If no part of the container has the focus, this
   --| function returns 0. The `Move_Focus` moves the focus to the part whose index is given
   --| in the `Part` parameter.

   --| The default implementations ...

   --\
   --------------------------------------------------------------------------------------------
   --/ :

   --\

















   --------------------------------------------------------------------------------------------
   --/ Command procedures:
   
   type Command_Parameters is abstract tagged null record;
   
   constant Null_Parameters: Command_Parameters'Class;

   type Command_Procedure is access procedure(Activator:  access Interactive_Component'Class;
                                              Parameters: in     Command_Parameters'Class := Null_Parameters);

   --| A 'command procedure' is a procedure which does something the user may wish to do as
   --| part of his or her interaction with a GUI application program.
   
   --| A command procedure is always called by an interactive component, which is called the
   command procedure's 'activator'. The activator can pass parameters to the command procedure 
   as a value of a type derived from the abstract type `Command_Parameters`.

   --\
   --------------------------------------------------------------------------------------------
   --/ Buttons:

   type Root_Button_Type is abstract new Interactive_Component with private;

   --| A 'button' can be made to perform some action or to select (or rescind) some option,
   --| by a user action, for example clicking the mouse on the button, or if the button has the
   --| focus by pressing the space bar. Examples of buttons include: command buttons
   --| (push-buttons); choice (radio) buttons; toggle buttons (check boxes).

   function Command (Button: in Root_Button_Type) return Command_Procedure;

   procedure Set_Command (Button:  in out Root_Button_Type
                          Command: in     Command_Procedure);

   --| Every button has a 'command' property, which is a command procedure. Whenever the
   --| button is activated, in addition to (and just after) any other actions inherent in the
   --| button's specific behaviour, the button's command is executed. The command access value
   --| may be null, in which case nothing is done when the command would be executed.

   --| The default implementation of `Set_Command` simply remembers the given command access
   --| value; the default implementation of `Command` returns this value.

--   function Label (Button: in Root_Button_Type) return Universal_String;
--
--   procedure Set_Label (Button: in out Root_Button_Type
--                        Label:  in     Universal_String);
--
--   function Icon (Button: in Root_Button_Type) return Image_Access;
--
--   procedure Set_Icon (Button: in out Root_Button_Type
--                       Icon:   in     Image_Access);

   --\
   --------------------------------------------------------------------------------------------
   --/ Indicator buttons:

   type Indicator_Button is abstract new Root_Button_Type with private;

   --| An 'indicator button' is able to visually show whether it has been 'indicated'
   --| (chosen, selected) or not. Examples of indicator buttons include: choice (radio)
   --| buttons; toggle buttons (check boxes).

   function is_Indicated (Button: in Root_Button_Type) return Boolean;

   procedure Set_Indicated (Button:    in out Root_Button_Type;
                            Indicated: in     Boolean := True);

   --| Every indicator button has the 'indicated' property. Note that the `Indicated`
   --| parameter of the `Set_Indicated` procedure has a convenient default value of `True`.

   --| The default implementation os `Set_Indicated` simply remembers the given value of
   --| `Indicated`; the default implementation os `Indicated` returns this value.

   --\
--   --------------------------------------------------------------------------------------------
--   --/ :
--
--   type Menu_Item is abstract tagged private;
--
--   --| A menu item is any of the possible entries in a menu. The usual kinds of menu item are:
--   --| a named command; a named sub-menu; a divider (line).
--
--   type Menu_Divider is new Menu_Item with null record;
--
--   type Menu_Option is new Menu_Item with
--      record
--         Name:        Wide_String;
--         Accelerator: ???;
--         Shortcut:    ???;
--         Icon:        ???;
--      end record;
--
--
--   type Menu_Command is new Menu_Option with
--      record
--         Action: Command_Procedure;
--      end record;
--
--   --\
--   --------------------------------------------------------------------------------------------
--   --/
--
--   type Menu_Item_Access is access all Menu_Item'Class;
--
--   type Menu_Descriptor is array (Positive range <>) of Menu_Item_Access;
--
--   --\
--   --------------------------------------------------------------------------------------------
--   --/ :
--
--   type Submenu (Count: Natural) is new Menu_Option with
--      record
--         Items: Menu_Descriptor(1..Count);
--      end record;
--
--   --\
   --------------------------------------------------------------------------------------------
   --/ :

   type Root_Frame_Type is abstract new Interactive_Container with private;

   --| A 'frame' is a simple kind of interactive container that draws a neat border around
   --| some other container, which is called the frame's 'canvas', and can also draw a
   --| textual title (typically near the top of the border), called the frame's 'label'.

   function Canvas (Frame: in Root_Frame_Type) return Component_Access;

   procedure Set_Canvas (Frame:  in out Root_Frame_Type
                         Canvas: in     Component_Access);

   function Label (Frame: in Root_Frame_Type) return Universal_String;

   procedure Set_Label (Frame: in out Root_Frame_Type
                        Label: in     Universal_String);

   --| Every frame has a canvas and label property.

   function Canvas_Position (Frame: in Root_Frame_Type) return Orthorect is abstract;

   --| The `Canvas_Position` function returns the extent of the canvas compnent relative to
   --| the frame's extent. The size (width and height) of this extent will (must) be the same
   --| as the canvas' extent, but it may be offset from it.

   function Margin (Frame: in Root_Frame_Type) return Absolute_Pixels;

   procedure Set_Margin (Frame:  in out Root_Frame_Type;
                         Margin: in     Absolute_Pixels);

   --| Every frame has a 'margin' property, which determines the thickness of the border, and
   --| therefore also determines the size of the (extent of the) canvas in relation to the size
   --| of the (extent of the) frame.

   --| The width of the canvas will be the width of the frame minus twice the margin;
   --| similarly, the height of the canvas will be the height of the frame minus twice the
   --| margin.

   --\
   --------------------------------------------------------------------------------------------
   --/ Text displays:

   type Text_Display is abstract new Root_Component_Type with private;



   function Formatting (Display: in out Text_Display) return Text.Formatting.Formatting_Scheme;

   procedure Set_Formatting (Display: in out Text_Display;
                             Scheme:  in     Text.Formatting.Formatting_Scheme);

   function Text (Display: in out Text_Display) return Universal_String;

   procedure Set_Text (Display: in out Text_Display;
                       Text:    in     Universal_String);





   --| The `Line_Length` of the formatting scheme of a text display is used for the
   --| computation of a suggested width or height (depending on the text direction), as
   --| interrogated by the `Suggested_Width` and `Suggested_Height` functions.

   --\
   --------------------------------------------------------------------------------------------
   --








   --| Also: Command_Button; Menu_Button; Toggle_Button; Choice_Button; Text_Editor;
   --| Horizontal_Scrollbar; Vertical_Scrollbar; Pan_Button; Choice_Selector; Toggle_Lister;
   --| Choice_Lister; Horizontal_Slider; Vertical_Slider; Numeric_Incrementer;
   --| Scrolling_Viewport; Horizontal_Divider; Vertical_Divider. Probably many more.



private

   type Root_Component_Type is abstract new Limited_Controlled with
      record
         Extent:        Orthorect                := ((0,0),(-1,-1));
         Context:       Component_Context_Access := null;
         Default_Color: Basic_Color              := (0.2,0.2,0.2); -- dark grey initial colour
      end record;

--   procedure Initialize (Component: in out Root_Component_Type);
--   procedure Finalize   (Component: in out Root_Component_Type);
-- not necessary (I think)

   type Control_Component is abstract new Root_Component_Type with
      record
         Activation: Activation_State := Deactivated;
      end record;

   type Interactive_Component is abstract new Control_Component with
      record
         Has_Focus: Boolean := False;
      end record;

   type Viewport_Component is abstract new Interactive_Component with
      record
         Canvas: Component_Access := null;
      end record;

   package Container_Part_Storage is new AI302.Containers.Vectors(Integer,Component_Access);

   type Interactive_Container is abstract new Interactive_Component with
      record
         Parts: Container_Part_Storage.Vector;
         Focus: Natural := 0; -- index of part which has focus, or 0 if none
      end record;

   type Root_Button_Type is abstract new Interactive_Component with
      record
         Command: Command_Procedure := null;
      end record;

   type Indicator_Button is abstract new Root_Button_Type with
      record
         Is_Indicated: Boolean := False;
      end record;

   type Root_Frame_Type is abstract new Interactive_Container with
      record
         Canvas: Component_Access       := null;
         Label:  Wide_Unbounded_String; -- initially null
         Margin: Absolute_Pixels        := 5;
      end record;

   type Text_Display is abstract new Root_Component_Type with
      record
         Formatting: Text.Formatting.Formatting_Scheme := Text.Formatting.Default_Formatting;
         Text:       Wide_Unbounded_String;            -- initially null
      end record;

end Titian.Components;


-----------------------------------------------------------------------------------------------
--/ LEGAL INFORMATION:

--| The "Titian Graphics Library", or "Titian", is a "Program" as defined in clause 0 of the
--| GPL, and its source code exactly comprises the contents of the files named in the
--| accompanying file "manifest.txt".

--| "Titian" is free software; you can redistribute it and/or modify it under the terms of the
--| GNU General Public License as published by the Free Software Foundation; either version 2,
--| or (at your option) any later version. Titian is distributed in the hope that it will be
--| useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--| FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You
--| should have received a copy of the GNU General Public License distributed with Tenet; see
--| the accompanying file "GPL.TXT". If not, write to:

--    Free Software Foundation
--    59 Temple Place - Suite 330
--    Boston, MA 02111-1307, USA

--| or visit the web site:

--    http://www.gnu.org/copyleft/

--| As a special exception, if other files instantiate generics from this unit, or you link
--| this unit with other files to produce an executable, this unit does not by itself cause the
--| resulting executable to be covered by the GNU General Public License. This exception does
--| not however invalidate any other reasons why the executable file might be covered by the
--| GNU General Public License.

--\
-----------------------------------------------------------------------------------------------
--/ About the Titian Graphics Library:

--| This library is intended to provide a generalised interface to graphics devices and
--| graphical software subsystems. It can be used for output to devices and display subsystems
--| that comprises more sophisticated facilities than plain text, such as the control of text
--| font design, size, and the positioning of text, as well as the drawing of point-markers,
--| lines and shapes, the control of the colours used for the foregoing, and the display of
--| (simulated) photographic images. It can also be used with interactive devices and
--| subsystems to provide all the familiar elements of a Graphic User Interface. Titian is
--| carefully designed to be practical for systems ranging from small dedicated-application
--| devices to the most powerful graphics workstations.

--| Please see the accompanying documentation for more information about this software.

--\
-----------------------------------------------------------------------------------------------
--/ Repository Data:

-- $Id$
-- $Name$

-- $Revision$
-- $Author$
-- $Date$
-- $State$

-- $Source$
-- $RCSfile$

-- $Log$
--

--\

--\
-----------------------------------------------------------------------------------------------
-- End of File.
