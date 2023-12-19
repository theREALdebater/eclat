-----------------------------------------------------------------------------------------------
--/ Titian.Raster_IO package specification:

--| Copyright (C) 2012 Nicholas James Roberts (South Croydon, Surrey, UK).

--| Part of the Titian Graphics Library. See the bottom (end) of this file for important legal
--| information.

-----------------------------------------------------------------------------------------------
with Titian.Color, Titian.Raster_Geometry;

use Titian.Color, Titian.Raster_Geometry;

-----------------------------------------------------------------------------------------------
package Titian.Raster_IO is

   pragma Pure;

   --------------------------------------------------------------------------------------------
   --/ The root context type:

   type Root_Context_Type is abstract tagged limited private;

   --| The type `Root_Context_Type` represents a 'drawing context', and is the fundamental
   --| abstract type for all raster graphics operations. This type represent any object upon
   --| which it is possible to draw (text, lines, points, etc.).

   --| The context is assumed to be a flat, two-dimensional drawing surface, which extends
   --| infinitely in all directions. The positioning of things to be drawn on the context uses
   --| raster geometry (see the package 'Titian.Raster_Geometry').

   --| It is undefined how a context causes things drawn on its surface to become apparent. In
   --| particular, it is undefined whether the (real-life, physical) mechanism used is limited
   --| in its resolution to the pixels; it may be finer or coarser, or the concept of a raster
   --| resolution may be irrelevant. However, Titian assumes that the positioning of things to
   --| be drawn is in terms of pixel addresses.

   --\
   --------------------------------------------------------------------------------------------
   --/ Actual scale:

   function Pixel_Width  (Context: Root_Context_Type) return Inches is abstract;
   function Pixel_Height (Context: Root_Context_Type) return Inches is abstract;

   function Pixel_Width  (Context: Root_Context_Type) return Centimeters is abstract;
   function Pixel_Height (Context: Root_Context_Type) return Centimeters is abstract;

   --| The functions `Pixel_Width` and `Pixel_Height` return the real-life physical size of
   --| each pixel in the raster of a root context. Pixel width is the same as column width, and
   --| pixel height is the same as row width.

   function Aspect_Ratio (Context: Root_Context_Type) return Float is abstract; -- X/Y
 
   --| The function `Aspect_Ratio` returns the ratio of pixel width to pixel height. If the
   --| pixels are exactly square, the result will be precisely 1.0; if they are slightly wider
   --| than their height, the result will be a little greater than 1.0.

   --\
   --------------------------------------------------------------------------------------------
   --/ Origin, clipping, and extent:

   function Clipping (Context: Root_Context_Type) return Orthorect is abstract;
   function Extent   (Context: Root_Context_Type) return Orthorect is abstract;

   --| The 'origin' of a context is the pixel whose address is (0,0).

   --| A context is assumed to have boundaries beyond which is it never possible or permitted
   --| to draw, which is called the 'extent' of the context. The extent is assumed to be an
   --| orthogonal rectangular. The extent applies the same rows and columns of the context
   --| regardless of their numbering. (It is assumed that the extent will correspond to some
   --| physical constraint of the context's display mechanism. It is not defined whether the
   --| real boundaries correspond exactly to the rows and columns, but if they don't then the
   --| real boundaries should lie outside the extent.)

   --| There is another orthogonal rectangle, called the 'clipping' of the context, which
   --| places a constraint on the drawing of things on the context. In theory, when anything is
   --| drawn, any part of it (however small) which lies outside the current clipping

   procedure Set_Origin (Context:   in out Root_Context_Type;
                         Principal: in     Principal_Point;
                         Offset:    in     Raster_Offset := (0,0)) is abstract;

   --| The procedure `Set_Origin` may change the numbering of the rows and/or columns of a
   --| context. If the point p is the principal point, according to `Principal`, of the
   --| context's extent, and d is the value of `Offset`, then the number of every row has the
   --| row offset of p+d subtracted from it, and the number of every column has the column
   --| offset of p+d subtracted from it. This has the effect of making the origin the point
   --| p+d; the clipping, extent, and default point are all offset by -(p+d) in order to prevent 
   them from being effectively moved.

   procedure Move_Origin (Context: in out Root_Context_Type;
                          Offset:  in     Raster_Offset) is abstract;

   --| The procedure `Move_Origin` may change the numbering of the rows and/or columns of a
   --| context. The number of every row has the row offset of `Offset` subtracted from it, and
   --| the number of every column has the column offset of `Offset` subtracted from it. This
   --| has the effect of adding `Offset` to the origin, as well as the extent, the clipping, and the default point of the
   --| context (to prevent them from being effectively moved).

   procedure Set_Clipping (Context: in out Root_Context_Type;
                           Region:  in     Orthorect) is abstract;

   --| The procedure `Set_Clipping` may change the clipping of a context. If any part of the
   --| given `Region` lies outside the context's extent, the exception `Clipping_Error` is
   --| propagated, otherwise the clipping is set to `Region`.

   procedure Set_Clipping_To_Extent (Context: in out Root_Context_Type) is abstract;

   --| The procedure `Set_Clipping_To_Extent` sets the clipping of a context to its extent (the
   --| maximum rectangle the clipping is permitted to be). In a sense, it turns clipping off.

   --\
   --------------------------------------------------------------------------------------------
   --/

   --\


   --------------------------------------------------------------------------------------------
   --/ Color operations:

   --------------------------------------------------------------------------------------------
   --/ Testing for direct color:

   function Has_Direct_Color (Context: Root_Context_Type) return Boolean is abstract;

   --\
   --------------------------------------------------------------------------------------------
   --/ Native palette:

   function Palette (Context: Root_Context_Type) return Root_Palette_Access is abstract;

   --\
   --------------------------------------------------------------------------------------------
   --/ Current color:

   procedure Set_Current_Color (Context: in out Root_Context_Type;
                                Color:   in     Color_Selector) is abstract;

   procedure Set_Current_Color (Context: in out Root_Context_Type;
                                Color:   in     Basic_Color) is abstract;

   function Current_Color (Context: in Root_Context_Type) return Color_Selector is abstract;
   function Current_Color (Context: in Root_Context_Type) return Basic_Color    is abstract;

   --\
   --------------------------------------------------------------------------------------------
   --/ Actual color:

   function Actual_Color (Color:   in Basic_Color;
                          Context: in Root_Context_Type) return Basic_Color is abstract;

   function Actual_Color (Selector: in Color_Selector;
                          Context:  in Root_Context_Type) return Basic_Color is abstract;

   function Actual_Current_Color (Context: in Root_Context_Type) return Basic_Color is abstract;

   --\
   --------------------------------------------------------------------------------------------
   --/ Default colors:

   function Default_Background_Color (Context: in Root_Context_Type) return Basic_Color is abstract;
   function Default_Drawing_Color    (Context: in Root_Context_Type) return Basic_Color is abstract;

   --\
   --\
   --------------------------------------------------------------------------------------------
   --/ Default point:

   procedure Set_Default_Point (Context: in out Root_Context_Type;
                                Point:   in     Raster_Point) is abstract;

   procedure Move_Default_Point (Context: in out Root_Context_Type;
                                 Offset:  in     Raster_Offset) is abstract;

   function Default_Point (Context: Root_Context_Type) return Raster_Point is abstract;

   --\
   --------------------------------------------------------------------------------------------
   --/ Drawing pointmarks:

   --\
   --------------------------------------------------------------------------------------------
   --/

   --\
   --------------------------------------------------------------------------------------------
   --/ Font operations:

   function New_Typeface_Iterator (Context:  in Root_Context_Type;
                                   Category: in String := "") return Typeface_Iterator;

   function Default_Font_Size ???

   type Font_Access is access all Root_Font_Type;

   function New_Native_Font (Context:  in Root_Context_Type;
                             Typeface: in Typeface_Reference := null;
                             Size:     in Font_Size          := 10.0;
                             Lining:   in Lining_Scheme      := No_Lining;
                             Spacing:  in Spacing_Scheme     := Normal_Spacing;
                             Rotation: in Text_Rotation      := Not_Rotated) return Font_Access is abstract;

   function New_Native_Font (Context: in Root_Context_Type;
                             Font:    in Root_Font_Type'Class) return Font_Access is abstract;

   procedure Set_Typeface (Context: in out Root_Context_Type;
                           Font:    in out Root_Font_Type'Class;
                           Name:    in     String);

   procedure Set_Typeface (Context: in out Root_Context_Type;
                           Font:    in out Root_Font_Type'Class;
                           Name:    in     String;
                           Found:   out    Boolean);

   procedure Set_Typeface (Context: in out Root_Context_Type;
                           Font:    in out Root_Font_Type'Class;
                           Name:    in     String;
                           Face:    out    Typeface_Reference);

   --\
   --------------------------------------------------------------------------------------------
   --/ Drawing text at the default point:

   --------------------------------------------------------------------------------------------
   --/ Current color:

   procedure Draw_Text (Context:   in out Graphics_Context;
                        Character: in     Universal_Character;
                        Font:      in     Root_Font_Type'Class;
                        Offset:    in     Raster_Offset := (0,0);
                        Rotation:  in     Text_Rotation := Not_Rotated);

   procedure Draw_Text (Context: in out Graphics_Context;
                        String:  in     Formatted_String;
                        Font:    in     Root_Font_Type'Class);

   --\
   --------------------------------------------------------------------------------------------
   --/ Color selector:

   procedure Draw_Text (Context:   in out Graphics_Context;
                        Character: in     Universal_Character;
                        Font:      in     Root_Font_Type'Class;
                        Color:     in     Color_Selector;
                        Offset:    in     Raster_Offset := (0,0);
                        Rotation:  in     Text_Rotation := Not_Rotated);

   procedure Draw_Text (Context: in out Graphics_Context;
                        String:  in     Formatted_String;
                        Font:    in     Root_Font_Type'Class;
                        Color:   in     Color_Selector);

   --\
   --------------------------------------------------------------------------------------------
   --/ Direct color:

   procedure Draw_Text (Context:   in out Graphics_Context;
                        Character: in     Universal_Character;
                        Font:      in     Root_Font_Type'Class;
                        Color:     in     Basic_Color;
                        Offset:    in     Raster_Offset := (0,0);
                        Rotation:  in     Text_Rotation := Not_Rotated);

   procedure Draw_Text (Context: in out Graphics_Context;
                        String:  in     Formatted_String;
                        Font:    in     Root_Font_Type'Class;
                        Color:   in     Basic_Color);

   --\
   --\
   --------------------------------------------------------------------------------------------
   --/ Drawing text at a specific point:

   --------------------------------------------------------------------------------------------
   --/ Current color:

   procedure Draw_Text (Context:   in out Graphics_Context;
                        Character: in     Universal_Character;
                        Point:     in     Raster_Point;
                        Font:      in     Root_Font_Type'Class;
                        Rotation:  in     Text_Rotation := Not_Rotated);

   procedure Draw_Text (Context: in out Graphics_Context;
                        String:  in     Formatted_String;
                        Point:   in     Raster_Point;
                        Font:    in     Root_Font_Type'Class);

   --\
   --------------------------------------------------------------------------------------------
   --/ Color selector:

   procedure Draw_Text (Context:   in out Graphics_Context;
                        Character: in     Universal_Character;
                        Point:     in     Raster_Point;
                        Font:      in     Root_Font_Type'Class;
                        Color:     in     Color_Selector;
                        Rotation:  in     Text_Rotation := Not_Rotated);

   procedure Draw_Text (Context: in out Graphics_Context;
                        String:  in     Formatted_String;
                        Point:   in     Raster_Point;
                        Font:    in     Root_Font_Type'Class;
                        Color:   in     Color_Selector);

   --\
   --------------------------------------------------------------------------------------------
   --/ Direct color:

   procedure Draw_Text (Context:   in out Graphics_Context;
                        Character: in     Universal_Character;
                        Point:     in     Raster_Point;
                        Font:      in     Root_Font_Type'Class;
                        Color:     in     Basic_Color;
                        Rotation:  in     Text_Rotation := Not_Rotated);

   procedure Draw_Text (Context: in out Graphics_Context;
                        String:  in     Formatted_String;
                        Point:   in     Raster_Point;
                        Font:    in     Root_Font_Type'Class;
                        Color:   in     Basic_Color);

   --\
   --\
   --------------------------------------------------------------------------------------------
   --/

   --\
   --------------------------------------------------------------------------------------------
   --/

   --\
   --------------------------------------------------------------------------------------------
   --/

   --\
   --------------------------------------------------------------------------------------------
   --/

   --\

end Titian.Raster_IO;

--\
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
