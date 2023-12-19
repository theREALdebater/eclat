-----------------------------------------------------------------------------------------------
--/ Titian.Raster_Geometry package specification:

--| Copyright (C) 2012 Nicholas James Roberts (South Croydon, Surrey, UK).

--| Part of the Titian Graphics Library. See the bottom (end) of this file for important legal
--| information.

-----------------------------------------------------------------------------------------------
with ???;


-----------------------------------------------------------------------------------------------
package Titian.Raster_Geometry is

   pragma Pure;

   --| A 'raster' is a conceptual matrix of cells called pixels ('picture elements'). Each
   --| pixel is intended to be square (but the possibility of pixels being slightly non-square
   --| is accommodated), and the pixels are all the same size. Each row of the raster has the
   --| same number of columns, and each column has the same number of rows, and the rectangle
   --| is called an 'orthogonal rectangle' ('orthorect' for short).

   --| Every row of the raster is uniquely identified by an integer; the row above it has the
   --| number one less, and the row below it has the number one greater. Similarly, the columns
   --| are uniquely identified by an integer, the column to its left has the number one less,
   --| and the column to its right has the number one greater. Any of the imaginary pixels of
   --| the raster can be uniquely identified by two integers, said to be the pixel's 'row
   --| number' and 'column number'; this is termed the pixel's 'address'.

   --\
   --------------------------------------------------------------------------------------------
   --/ Linear measures:

   type Relative_Pixels is new Integer; -- implementation-defined integer type
 
   --| The type `Relative_Pixels` is a signed integer type which measures the offset between
   --| any two rows of a raster, or the offset between any two columns of a raster.

   subtype Absolute_Pixels is Relative_Pixels range 0..Relative_Pixels'Last;

   --| The type `Absolute_Pixels` is a signed integer type which measures the number of rows
   --| between any two rows of a raster, or the number of columns between any two columns of a
   --| raster.

   --\
   --------------------------------------------------------------------------------------------
   --/ Distances, coordinates, points, and offsets:

   subtype Raster_Coordinate is Relative_Pixels
      range Relative_Pixels'First/2..Relative_Pixels'Last/2; -- implementation defined range

   --| The subtype `Raster_Coordinate` represents the number of a row or column on a raster.

   type Raster_Point is
      record
         X, Y: Raster_Coordinate;
      end record;

   --| The type `Raster_Point` represents a pixel address, and comprises a column number (`X`)
   --| and a row number (`Y`).

   type Raster_Offset is
      record
         X, Y: Relative_Pixels;
      end record;

   --| The type `Raster_Offset` represents the two-dimensional offset between two pixels, and
   --| comprises a horizontal offset (`X`) and a vertical offset (`Y`).

   --\
   --------------------------------------------------------------------------------------------
   --/ Point and offset operations:

   function "+"(Right: Raster_Offset) return Raster_Offset;
   function "-"(Right: Raster_Offset) return Raster_Offset;
 
   --| The unary operator "+" of a raster offset returns the same offset unchanged. The unary
   --| operator "-" of a raster offset returns the inverse offset. (If y is the point resulting
   --| from adding an offset d to a point x, and d' is the inverse of d, than adding d' to y
   --| gives x.)

   function "+"(Left: Raster_Point,  Right: Raster_Offset) return Raster_Point;
   function "+"(Left: Raster_Offset; Right: Raster_Point)  return Raster_Point;
   function "-"(Left: Raster_Point;  Right: Raster_Offset) return Raster_Point;
 
   --| The binary operator "+" of a raster point p and a raster offset d results in a point
   --| whose row number is that of p plus the row offset of d and whose column number is that
   --| of p plus the column offset of d. The binary operator "+" of a raster offset d and a
   --| raster point p is the same. The binary operator "+" of a raster point p and a raster
   --| offset d results in a point whose row number is that of p minus the row offset of d and
   --| whose column number is that of p minus the column offset of d.

   function "-"(Left, Right: Raster_Point) return Raster_Offset;
 
   --| The binary operator "-" of two raster points results in a raster offset whose row offset
   --| is the difference between the row numbers of the two points, and whose column offset is
   --| the difference between the the column numbers of the two points.

   function "+"(Left, Right: Raster_Offset) return Raster_Offset;
   function "-"(Left, Right: Raster_Offset) return Raster_Offset;

   --| The binary operator "+" of two raster offsets is the composition of the two raster
   --| offsets; it results in a raster offset whose row offset is the addition of the row
   --| offsets of the two raster offsets, and whose column offset is the addition of the column
   --| offsets of the two raster offsets. The binary operator "-" of two raster offsets is the
   --| difference of the two raster offsets; it results in a raster offset whose row offset is
   --| the difference between the row offsets of the two raster offsets, and whose column
   --| offset is the difference between the column offsets of the two raster offsets.

   --\
   --------------------------------------------------------------------------------------------
   --/ Lines:

   type General_Line is
      record
         P1, P2: Raster_Point;
      end record;

   --| A 'line' is primarily characterised by two points, called its 'endpoints', but in
   --| reality it is used to represent various geometrical entities that would properly have a
   --| variety of mathematical terms which might be confusing to non-mathematicians. (For
   --| example, in maths, a line is considered to be curve. It's best if we don't use the term
   --| 'curve' in this context, because it's so likely to be confusing.) It is assumed that when a line is drawn
   --| it will be drawn as a straight line from one of the points to the other.

   type Horizontal_Line is
      record
         X1, X2, Y: Raster_Coordinate;
      end record;

   --| A 'horizontal line' is a line that is parallel with the x-axis. The y co-ordinate of the
   --| two endpoints is the same.

   type Vertical_Line is
      record
         X, Y1, Y2: Raster_Coordinate;
      end record;

   --| A 'vertical line' is a line that is parallel with the y-axis.The x co-ordinate of the two
   --| endpoints is the same.

   --| Separate types for horizontal and vertical lines are declared for the purposes of
   --| efficiency, since it is not necessary to store one of the co-ordinates twice.

   --\
   --------------------------------------------------------------------------------------------
   --/ Line testing and conversion:

   function To_General (Line: in Horizontal_Line) return General_Line;
   function To_General (Line: in Vertical_Line)   return General_Line;
 
   --| The function `To_General` converts either a horizontal or vertical line to a general
   --| line.

   function Is_Horizontal (Line: in General_Line) return Boolean;
   function Is_Vertical   (Line: in General_Line) return Boolean;

   --| The function `Is_Horizontal` returns `True` if a given general line is horizontal. The
   --| function `Is_Vertical` returns `True` if a given general line is vertical.

   --\
   --------------------------------------------------------------------------------------------
   --/ Orthogonal rectangles:

   type Orthorect is
      record
         TL, BR: Raster_Point; -- top left, bottom right
      end record;

   --| An 'orthogonal rectangle' is a geometric rectangle whose sides are parallel to the axes.
   --| It is primarily characterised by its top left corner (`TL`) and its bottom right corner
   --| (`BR`), which are both points (pixel addresses).

   --------------------------------------------------------------------------------------------
   --/ Normalization and nullity:

   procedure Normalize (Figure: in out Orthorect);

   function Normalize (Figure: Orthorect) return Orthorect;

   --| An orthoganal rectangle is 'normal' if TL.X <= BR.X and TL.Y <= BR.Y. Any orthoganal
   --| rectangle is normalized (made normal) by swapping TL.X and BR.X if TL.X > BR.X and then
   --| swapping TL.Y and BR.Y if TL.Y > BR.Y. The 'Normalize' function returns the normalized
   --| value of the given orthoganal rectangle; the 'Normalize' procedure causes the 'Figure'
   --| parameter to be replaced by its normalized value.

   function Is_Null (Figure: Orthorect) return Boolean;

   --| An orthogonal rectangle is considered 'null' if it is not normal (in other words TL.X >
   --| BR.X or TL.Y > BR.Y). The 'Is_Null' function returns 'True' if the given 'Figure' is
   --| null (and returns False otherwise).

   --\
   --------------------------------------------------------------------------------------------
   --/ width and height:

   function Width  (Figure: Orthorect) return Absolute_Pixels;
   function Height (Figure: Orthorect) return Absolute_Pixels;

   --| If the given orthogonal rectangle is null, both the 'Width' and 'Height' functions
   --| return 0. Otherwise, 'Width' returns the width of the rectangle, and 'Height' returns
   --| its height.

   --\
   --------------------------------------------------------------------------------------------
   --/ Principal points:

   function Point_At (Principal: in Principal_Point
                      Figure:    in Orthorect) return Raster_Point;

   --| The 'Point_At' functions returns the point at which the given principal point coincides
   --| with the given orthogonal rectangle.

   --\
   --------------------------------------------------------------------------------------------
   --/ Testing for exclusion:

   function is_Outside (Point:  Raster_Point;
                        Region: Orthorect) return Boolean;

   function is_Outside (Line:   General_Line;
                        Region: Orthorect) return Boolean;

   function is_Outside (Line:   Horizontal_Line;
                        Region: Orthorect) return Boolean;

   function is_Outside (Line:   Vertical_Line;
                        Region: Orthorect) return Boolean;

   function is_Outside (Figure, Region: Orthorect) return Boolean;

   --| The 'Is_Outside' functions return 'True' if any part of a given point, line, or
   --| orthogonal rectangle lies outside an orthogonal rectangle (given in the 'Region'
   --| parameter), and return 'False' otherwise.

   --\
   --------------------------------------------------------------------------------------------
   --/ Intersection:

   function Intersection (Figure_1, Figure_2: Orthorect) return Orthorect;

   --| If either of the given orthogonal rectangles is null, the result of the 'Intersection'
   --| is some null orthogonal rectangle (whose value is otherwise undefined), otherwise the
   --| result is the orthogonal rectangle which is the geometric intersection of the 'Figure_1'
   --| and 'Figure_2' parameters.

   --\
   --------------------------------------------------------------------------------------------
   --/ Offsetting:

   function "+" (Left: Orthorect; Right: Raster_Offset) return Orthorect;
   function "-" (Left: Orthorect; Right: Raster_Offset) return Orthorect;
 
   --| The binary operator "+" of an orthogonal rectangle and a raster offset returns an
   --| orthogonal rectangle whose corners both have the raster offset added to them. The binary
   --| operator "-" of an orthogonal rectangle and a raster offset returns an orthogonal
   --| rectangle whose corners both have the raster offset subtracted from them.

   --\
   --------------------------------------------------------------------------------------------
   --/ Surface coverage:

   type Surface_Coverage is private;

   --| A 'surface coverage' value is conceptually an aribitrary geometric subset of a
   --| two-dimensional surface. In practice, it is stored as a set of non-intersecting
   --| orthogonal rectangles. Operations are provided to construct and interrogate a surface
   --| coverage value in terms of orthogonal rectangles. (It can be thought of as a set of
   --| points or pixels.)
   
   --/ Normalisation and nullity:

   Null_Coverage: constant Surface_Coverage;

   --| The constant 'Null_Coverage' represents a value which is the (geometric) null set.

   function Is_Null (Source: in Surface_Coverage) return Boolean;

   --| The `Is_Null` function returns 'True' if the 'Source' parameter represents a (geometric)
   --| null set. (There are no points or pixels in the set.)

   --| It is important to note that it is permitted for there to be many surface coverage
   --| values which are null, so that `Is_Null(X)` and `Is_Null(Y)` does not necessarily imply `X =
   --| Y`, nor `X = Null_Coverage`, nor `Y = Null_Coverage`. However, it is guaranteed that
   --| `Normalize(X) = Normalize(Y)` and `Normalize(X) = Null_Coverage` and `Normalize(Y) =
   --| Null_Coverage`.

   procedure Clear (Source: in out Surface_Coverage);

   --| The 'Clear' procedure has the effect of assigning 'Null_Coverage' to the 'Source'
   --| parameter.

   function Normalize (Source: in Surface_Coverage) return Surface_Coverage;
 
   --| The 'Normalize' function returns the normalised form of a given surface coverage. The
   --| only difference between any surface coverage value and its normalised form is that the
   --| normalised form might be more efficient (in terms of the time taken to perform other
   --| operations on it, and the amount of sotrage space it uses up), unless the given surface
   --| coverage is null, in which case its normalised form is guaranteed to be (the value of)
   --| 'Null_Coverage'.
   
   --\
   --/ Geometric operations:

   function "not" (Right: in Surface_Coverage) return Surface_Coverage;

   --| The unary operator "not" returns the geometric inverse of the parameter, within the
   --| universe that is formed by the (orthogonal) rectangle whose top left point is
   --| '(Raster_Coordinate'First,Raster_Coordinate'First)' and whose bottom right point is
   --| '(Raster_Coordinate'Last,Raster_Coordinate'Last)'.

   function "and" (Left:  in Surface_Coverage;
                   Right: in Surface_Coverage) return Surface_Coverage;

   function "or"  (Left:  in Surface_Coverage;
                   Right: in Surface_Coverage) return Surface_Coverage;

   function "xor" (Left:  in Surface_Coverage;
                   Right: in Surface_Coverage) return Surface_Coverage;

   function "-"   (Left:  in Surface_Coverage;
                   Right: in Surface_Coverage) return Surface_Coverage;

   --| The '"and"', '"or"', '"xor"', and '"-"' functions return the geometric intersection,
   --| union, symmetric difference, and difference respectively of the two parameters.
   
   --\
   --/ Conversion to and from an array:

   type Orthorect_Array is array (Positive range <>) of Orthorect;

   --| The type 'Orthorect_Array' is used as a convenient representation of a surface coverage
   --| value, and conversion operations between these types are provided.

   function To_Array (Source: in Surface_Coverage) return Orthorect_Array;

   function To_Array (Source: in Surface_Coverage;
                      Clip:   in Orthorect) return Orthorect_Array;

   --| The 'To_Array' functions convert a surface coverage value into an equivalent array of
   --| orthogonal rectangles. The overloading with a 'Clip' parameter returns this conversion
   --| performed on the geometric intersection of the 'Source' and 'Clip' parameters.
   
   --\
   --/ Construction:

   function To_Surface_Coverage (Singleton: in Orthorect) return Surface_Coverage;

   --| The 'To_Surface_Coverage' function with an orthogonal rectangle parameter (named
   --| 'Singleton') returns a surface coverage value that corresponds to the rectangle. If the
   --| rectangle is null, the result is 'Null_Surface_Coverage'.

   function To_Surface_Coverage (Source: in Orthorect_Array) return Surface_Coverage;

   --| The 'To_Surface_Coverage' function with an 'Orthorect_Array' parameter (named 'Source')
   --| returns a surface coverage value that corresponds to the union of all the rectangles in
   --| the array. If this union results in an empty set, the result is some null surface
   --| coverage value (which may or may not be equal to 'Null_Surface_Coverage').
   
   --\
   --/ Combination:

   procedure Include (Source: in out Surface_Coverage;
                      Figure: in     Orthorect);

   --| The 'Include' procedure whose second parameter is an orthogonal rectangle (named
   --| 'Figure') assigns to the 'Source' parameter the union of the values of 'Source' and the
   --| rectangle.

   procedure Include (Source: in out Surface_Coverage;
                      Extra:  in     Surface_Coverage);

   --| The 'Include' procedure whose second parameter is a surface coverage value (named
   --| 'Extra') assigns to the 'Source' parameter the union of the values of 'Source' and
   --| 'Extra'.

   procedure Exclude (Source: in out Surface_Coverage;
                      Figure: in     Orthorect);

   --| The 'Exclude' procedure whose second parameter is an orthogonal rectangle (named
   --| 'Figure') assigns to the 'Source' parameter the difference of the values of 'Source' and
   --| the rectangle (specifically, 'Source-Figure').

   procedure Exclude (Source: in out Surface_Coverage;
                      Extra:  in     Surface_Coverage);

   --| The 'Exclude' procedure whose second parameter is a surface coverage value (named
   --| 'Extra') assigns to the 'Source' parameter the difference of the values of 'Source' and
   --| 'Extra' (specifically, 'Source-Extra').
   
   --\
   --/ Clipping:

   function Clip (Source:   in Surface_Coverage;
                  Boundary: in Orthorect) return Surface_Coverage;

   --| The 'Clip' function returns the geometric intersection of the values of the 'Boundary'
   --| and 'Source' parameters (specifically, 'Boundary and Source').

   procedure Clip (Source:   in out Surface_Coverage;
                   Boundary: in     Orthorect);

   --| The 'Clip' procedure assigns to the 'Source' parameter the geometric intersection of the
   --| values of the 'Boundary' and 'Source' parameters (specifically, 'Boundary and Source').
   
   --\
   --/ Offsetting:

   function "+" (Left:  in Surface_Coverage;
                 Right: in Raster_Offset) return Surface_Coverage;

   function "-" (Left:  in Surface_Coverage;
                 Right: in Raster_Offset) return Surface_Coverage;

   --| The '"+"' and '"-"' functions return the surface coverage value in the 'Left' parameter
   --| offset by the 'Right' parameter (as if each component orthogonal rectangle 'R' in the
   --| coverage were replaced by the rectangle 'R+Right' or 'R-Right' respectively).

   procedure Move (Source: in out Surface_Coverage;
                   Offset: in     Raster_Offset);

   --| The 'Move' procedure assigns to the 'Source' parameter the value of 'Source' offset by
   --| the 'Offset' parameter ('Source+Offset').
   
   --\
   --/ Iteration of component rectangles:

   type Orthorect_Processor is access procedure (Item: in Orthorect);

   procedure Iterate (Source:    in Surface_Coverage;
                      Processor: in Orthorect_Processor);

   type Orthorect_Mapping is access function (Input: in Orthorect) return Orthorect;

   procedure Transform (Source:  in Surface_Coverage;
                        Mapping: in Orthorect_Mapping);

   Surface_Coverage_Error: exception; -- raised if capacity exceeded

   --\
   --\
   --\
   --------------------------------------------------------------------------------------------

private

   --| The implementation of the `Surface_Coverage` type is based on a binary tree of
   --| orthogonal rectangles. There are two kinds of node: a 'search node'; a 'member node'.
   --| The member nodes are always 'leaves' of the tree (they have no descendants), and the
   --| search nodes are never leaves (each always has exacly two children). Each node (of
   --| either kind) contains one orthogonal rectangle. The geometric set that a value of the
   --| type represents is the union of the (orthogonal rectangles contained by) all the member
   --| nodes. The (orthogonal rectangles contained by) the two children of a search node always
   --| lie within the orthogonal rectangle contained by) the search node; therefore, all the
   --| member nodes which descend from a search node lie geometrically within the search node.
   --| The member nodes are all disjoint (none overlaps with any other). This arrangement
   --| allows the operations on surface coverage values to be performed with reasonable
   --| efficiency.

   type Node_Kind is (Member_Node, Search_Node);

   type Node_Descriptor (Kind: Node_Kind) is
      record
         case Kind is
            when Search_Node =>


   type Surface_Coverage is
      record
 

      
      
      
      
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
