-----------------------------------------------------------------------------------------------
--/ Titian.Colour package specification:

--| Copyright (C) 2004 Nicholas James Roberts (South Croydon, Surrey, UK).

--| Part of the Titian Graphics Library. See the bottom (end) of this file for important legal
--| information.

--| Titian uses a colour system which accommodates the fact that many computer display systems
--| can only show a limited number of different colours at a time, whilst others can show an
--| unlimited number. For the former kind of display systems, the set of colours which can be
--| displayed is generally called a 'palette'. The latter display systems are often described
--| as 'true colour' systems; the Titian term for them is that they have 'direct colour'.

--| Titian ensures that the programmer has the choice of using direct colour values or colours
--| selected from a palette, for all drawing operations. The former option will be slightly
--| more efficient for direct colour contexts, and may be more convenient regardless of the
--| context. The latter option will be more efficient for contexts which do not have direct
--| colour (but will be reasonably efficient for contexts that do). For maximum efficiency with
--| all possible contexts, the programmer could program two different algorithms for contexts
--| with and without direct colour.

--| The basic colour definitions are collected in this package (`Titian.Color`). This package
--| has the child packages `Titian.Color.CMYK` and `Titian.Color.HSL`. See also the package
--| 'Titian.Raster_IO', which contains the colour-related graphics context primitive
--| operations.

--| Notes:

--| Dealing with colour in a way which will be suitable for the widest range of devices
--| presents a bit of a challenge: different devices have widely varying ways of supporting
--| colour (e.g. from those which have no colours at all, to video adapters which support a
--| plethora of different modes, each with its own combination of bit-planes, palette sizes,
--| and so on). I think there is a necessity to support the sophisticated colour options many
--| devices provide - it would surely be a shame not to allow programs to take full advantage
--| of them - but not to the extent that overly complicates life for the poor beleaguered
--| application programmer. Furthermore, the facilities provided by the proposed binding for
--| colour manipulation must be reasonably efficient. To this end, I have devised a system
--| which hopefully allows programmers the best of both worlds.

--| I hope the term 'direct colour' (defined above) will be considered reasonably neutral.

--| I need help, in this section, with colour systems. Also, I am completely in the dark as to
--| the ins and outs of colour correction: I would appreciate help in this area.

--| - NJR

-----------------------------------------------------------------------------------------------
with ???;


-----------------------------------------------------------------------------------------------
package Titian.Color is

   pragma Pure;


   --------------------------------------------------------------------------------------------
   --/ Colour exception:

   Color_Error: exception;

   --| There is one colour-related exception, `Color_Error`, which is raised if:

   --| * an attempt to engage a colour in a palette fails (normally because the palette doesn't
   --| have enough free slots);

   --| * an attempt is made to use a colour selector for an invalid slot (one which is neither
   --| fixed nor has been engaged);

   --| * an attempt is made to release fixed colours;

   --| * a colour conversion cannot be effected, either because the calculation would be too
   --| difficult (or slow), or because the destination type cannot hold a colour value
   --| corresponding (closely enough) to the colour being converted.

   --| Details of when and where this exception is raised are given in the appropriate places
   --| in the rest of this package.

   --\
   --------------------------------------------------------------------------------------------
   --/ Basic colour descriptor:

   type Light_Level is delta 1.0/2**32 range 0.0 .. 1.0; -- implementation defined delta
   type Alpha_Level is delta 1.0/2**32 range 0.0 .. 1.0; -- implementation defined delta

   for Light_Level'Size use 32; -- implementation defined
   for Alpha_Level'Size use 32; -- implementation defined

   type Basic_Color is
      record
         Red, Green, Blue: Light_Level;
         Alpha: Alpha_Level := 1.0;
      end record;

   for Basic_Color'Size use 32*4; -- implementation defined

   --| The type `Basic_Colo`r holds colour values in the familiar Red-Green-Blue, or 'RGB',
   --| system of colour representation, with the addition of an 'alpha value', which can be
   --| used to control the extent to which the background of an image is allowed to show
   --| through. The RGB system with alpha values can be termed the 'RGBA' colour system.

   --\
   --------------------------------------------------------------------------------------------
   --/ Palette selectors and length:

   --| The term 'palette' is used for any set of distinct colours, as well as for the
   --| particular set of colours associated with a graphics context.

   type Color_Count is range 0..2**16-1; -- implementation defined upper limit

   subtype Color_Selector is Color_Count range 1..Color_Count'Last;
   subtype Palette_Length is Color_Count range 2..Color_Count'Last;

   --| Each place, in a palette, which holds a single colour is a 'slot' in that palette. Each
   --| slot in a palette is uniquely identified by a positive integer, called a 'colour
   --| selector', values of which are of the subtype Colour_Selector.

   --| The type `Color_Selector` is always implemented with a fairly large range (at least a
   --| few thousand). The subtype `Palette_Length` is appropriate for most objects which will
   --| determine the length of a palette, i.e. the number of colours it holds (since a palette
   --| with less than two colours would usually be nonsense).

   --\
   --------------------------------------------------------------------------------------------
   --/ Root palette type:

   type Root_Palette_Type is abstract tagged private;

   --| The abstract type `Root_Palette_Type` represents palettes in general, regardless of the
   --| details of how they are stored or implemented. This type is then 'solidified', by
   --| derivation, into the various real forms of palette representation.

   --\
   --------------------------------------------------------------------------------------------
   --/ Palette length:

   function Length (Palette: in Root_Palette_Type) return Palette_Length is abstract;

   --| Every palette has a 'palette length', which determines the number of slots (non-fixed
   --| slots, in fact) it has (and so the maximum number of colours it can hold). For most
   --| palettes, the length will be permananetly fixed. The function Length returns the
   --| (current) length of a palette.

   --| For palettes which store slots by means of an expanding structure (one which allocates
   --| memory on an as-needed basis), the length is the _maximum_ number of slots the palette
   --| can hold (not the current number allocated).

   --\
   --------------------------------------------------------------------------------------------
   --/ Fixed colours for a palette:

   --| Some palettes will have one or more slots containing pre-ordained colours, which cannot
   --| be changed. These slots are the palette's 'fixed colour slots', and their colours are
   --| its 'fixed colours'.

   --| The fixed colour slots of a palette, if any, always occupy a range of slots whose
   --| selectors are numbered contiguously; this contiguous range will always lie above the
   --| length of the palette, but is otherwise implementation defined.

   --| Like the length, the number of fixed colours of a palette is usually (itself) fixed, as
   --| is their range of selectors.

   function Fixed_Colors (Palette: in Root_Palette_Type) return Color_Count is abstract;

   function First_Fixed_Color (Palette: in Root_Palette_Type) return Color_Selector is abstract;
   function Last_Fixed_Color  (Palette: in Root_Palette_Type) return Color_Selector is abstract;

   --| The function `Fixed_Colors` returns the (current) number of fixed colours of a palette.
   --| The functions `First_Fixed_Color` and `Last_Fixed_Color` return the (current) selector
   --| of the first and last fixed colour, respectively; if there are no fixed colours, these
   --| two functions return an arbitrary range where the last is one less than the first.

   function Is_Fixed (Selector: in Color_Selector;
                      Palette:  in Root_Palette_Type) return Boolean is abstract;

   --| The utility function `Is_Fixed` tests whether a particular slot is a fixed colour slot.

   --| Fixed colour slots cannot be set, engaged, or released: any attempt to set, engage, or
   --| release a fixed colour slot (by a call to one of the procedures `Set_Slot`, `Engage`, or
   --| `Release`) will raise the exception `Color_Error` instead.

   --\
   --------------------------------------------------------------------------------------------
   --/ Palette slot engagement tests:

   --| The number of non-fixed slots actually holding colours, at any one time, in a palette,
   --| can vary dynamically, between zero and the length of the palette. These slots are
   --| 'engaged'. The selectors of the engaged slots in a palette are always in a contiguous
   --| range, from one up to the number of colours engaged.

   function Engaged_Slots (Palette: in Root_Palette_Type) return Color_Count is abstract;

   --| The `Engaged_Slots` function returns the number of slots currently engaged in a palette.

   function Is_Engaged (Selector: in Color_Selector;
                        Palette:  in Root_Palette_Type) return Boolean is abstract;

   --| The utility function `Is_Engaged` tests whether a slot is currently engaged (i.e.
   --| whether its selector is less than or equal to the current number of engaged slots).

   function Free_Slots (Palette: in Root_Palette_Type) return Color_Count is abstract;

   --| The utility function `Free_Slots` returns the number of slots of a palette still
   --| available to be engaged (i.e. the palette's length minus the number of engaged slots).
   --| These are the palette's 'free slots'.

   --\
   --------------------------------------------------------------------------------------------
   --/ Palette slot validity:

   function Is_Valid (Selector: in Color_Selector;
                      Palette:  in Root_Palette_Type) return Boolean is abstract;

   --| The utility function `Is_Valid` tests whether a slot is currently a 'valid slot'. A slot
   --| is valid if it is either fixed or engaged.

   --| For all calls of the primitive operations of both general palettes and graphics contexts
   --| taking a parameter which is a colour selector, if the value of the selector is invalid
   --| the exception `Color_Error` is raised, and, if the operation is a procedure, no other
   --| action is taken by the procedure. This excludes the `Is_Fixed`, `Is_Engaged`, and
   --| `Is_Valid` functions described just above (which simply return `False` instead).

   --\
   --------------------------------------------------------------------------------------------
   --/ Retrieval and setting of palette slots:

   function Slot_Value (Selector: in Color_Selector;
                        Palette:  in Root_Palette_Type) return Basic_Color is abstract;

   --| The `Slot_Value` function retrieves the colour held in a specific slot of a palette, and
   --| returns the value of that colour, as a basic colour value. The slot selected must be
   --| valid. If the selector passed in is not valid the exception `Color_Error` is raised.

   procedure Set_Slot (Palette:  in out Root_Palette_Type;
                       Selector: in     Color_Selector;
                       Color:    in     Basic_Color) is abstract;

   --| The `Set_Slot` procedure sets the colour held in a specific (non-fixed) slot of a
   --| palette; the colour is provided as a basic colour value. The slot selected must be
   --| engaged. If the selector passed in is not engaged (i.e. greater than the number of slots
   --| engaged) the exception `Color_Error` is raised, and the value of the selected slot
   --| remains unchanged.

   --\
   --------------------------------------------------------------------------------------------
   --/ Engaging palette slots:

   procedure Engage (Palette:  in out Root_Palette_Type;
                     Color:    in     Basic_Color) is abstract;

   procedure Engage (Palette:  in out Root_Palette_Type;
                     Color:    in     Basic_Color;
                     Selector: out    Color_Selector) is abstract;

   --| The two overloaded `Engage` procedures engage a colour into a palette. Whether this is
   --| done by always engaging one new slot (and setting its initial value to the given
   --| colour), or by first searching the palette to see if a slot already contains the given
   --| colour, is implementation defined (as is the method of search, if used).

   --| The second procedure differs only in that it passes out the selector of the engaged slot
   --| (whether it is a slot that already contained the colour and has been re-used, or is a
   --| newly engaged slot).

   --| Obviously, searching, if used, could be a very time-inefficient method. The method used
   --| for the search is implementation defined, but one possible method is simply to search
   --| from the end of the palette backwards. Provided that in most cases a colour which is
   --| engaged has already been recently engaged, this technique is likely to prove reasonably
   --| efficient.

   --| If there are no free slots when either procedure is called, and a free slot was
   --| required, it raises `Color_Error`, and no engagement takes place.

   --| If a free slot is required, but not available, an implementation is allowed to perform
   --| an optimization (see >Palette Optimization<), without reducing the number of engaged
   --| colours (quantization), in an attempt to obtain a free slot, before raising the
   --| exception.

   --| Note: Should this automatic 'merging' (removing duplicate colours from a palette) be
   --| provided for, or would this be better left to the programmer? NJR

   --\
   --------------------------------------------------------------------------------------------
   --/ Releasing palette slots:

   procedure Release (Palette: in out Root_Palette_Type;
                      First:   in     Color_Selector := 1) is abstract;

   --| The `Release` procedure 'releases' engaged slots.

   --| Any engaged slots whose selectors are greater than or equal to the parameter `First`
   --| cease to be engaged, and become free (i.e. the number of colours engaged becomes `First`
   --| minus one). If there are no such engaged colours (i.e. the number of engaged colors is
   --| less than `First`), this procedure does nothing (there is no error).

   --| The default value of one for the `First` parameter is convenient for the frequent case
   --| where the whole of the (non-fixed part of the) palette is to be released.

   --\
   --------------------------------------------------------------------------------------------
   --/ Palette data extraction:

   procedure Extract (Source: in     Root_Palette_Type;
                      Target: in out Root_Palette_Type'Class;
                      First:  in     Color_Selector;
                      Last:   in     Color_Selector) is abstract;

   procedure Extract (Source: in     Root_Palette_Type;
                      Target: in out Root_Palette_Type'Class;
                      First:  in     Color_Selector := 1) is abstract;

   --| The two overloaded procedures `Extract` select a range of slots from a general palette,
   --| parameter `Source`, and then enagage the colours in those slots into another palette,
   --| parameter `Target`. The colours are engaged in increasing selector order.

   --| Note: Should the order be implementation defined? NJR

   --| The first procedure selects the range specified by the parameters `First` to `Last`
   --| (inclusive). The second procedure selects the range from `First` to the number of
   --| engaged slots (of palette `Source`); again, there is a convenient default (one).

   --| The `Target` palette can be of any general palette type. Presumably, if it happens to be
   --| of the same type as the `Source`, the colours can be copied across directly; otherwise
   --| they will probably have to be converted to basic colours, and then engaged using the
   --| `Engage` procedure of the `Target` (generally a less efficient method).

   --| For both procedures, if any of the selected slots are not valid (in the `Source`
   --| palette), or if the number of slots required is greater than the number of free slots in
   --| the `Target` palette, the exception `Color_Error` is raised, and no engagement takes
   --| place. Again, the implementation is allowed (but not required) to optimise in an attempt
   --| to free more slots, if necessary.

   --\
   --------------------------------------------------------------------------------------------
   --/ Palette optimization:

   type Color_Selector_Remap is array (Color_Selector range <>) of Color_Selector;

   procedure Optimize (Palette: in out Root_Palette_Type) is abstract;

   procedure Optimize (Palette: in out Root_Palette_Type;
                       Remap:   out    Color_Selector_Remap) is abstract;

   procedure Optimize (Palette: in out Root_Palette_Type;
                       Maximum: in     Palette_Length) is abstract;

   procedure Optimize (Palette: in out Root_Palette_Type;
                       Maximum: in     Palette_Length;
                       Remap:   out    Color_Selector_Remap) is abstract;

   --| There are four `Optimize` procedures.

   --| The first two procedures cause a palette to eliminate duplicated colours (different
   --| engaged slots which contain the same colour value). This is called 'palette
   --| optimization'.

   --| Each duplicate colour is removed, by means of the remaining engaged colours being all
   --| 'shuffled down' one slot, or otherwise moved: this is implementation defined. However,
   --| the number of engaged colours ends up being reduced by the number of duplicate colours,
   --| and the duplicate colours disappear.

   --| Of course, this means that some (maybe all but the first) colours will end up in a
   --| different slot, and so with a different selector. The second procedure generates a
   --| 'remap array', which maps each colour's old selector to its corresponding new one. If
   --| the bounds of this array do not accomodate all the remapped selectors, the standard
   --| exception Constraint_Error is raised. All selectors in this array not remapped are
   --| mapped to themselves.

   --| The third and fourth Optimize procedures enable the number of engaged colours to be
   --| reduced to a specific number, the 'optimization maximum' (in the parameter `Maximum`).

   --| If ordinary optimization would cause the number of engaged colours to be less than or
   --| equal to the optimization maximum anyway, that is all these procedures do. Otherwise,
   --| pairs of colour values which are visually proximate - by some measure that is
   --| implementation defined - are 'amalgamated' into one colour (and the duplicate removed);
   --| this process continues until the number of engaged colours equals the maximum. (This
   --| process is sometimes termed 'quantization'.)

   --| For all these procedures, if the palette is empty (the numer of engaged slots is zero),
   --| calling the procedure does not nothing (and there is no error).

   --| Note: These procedures could be quite slow for palettes which have a large number of
   --| engaged colours. NJR

   --\
   --------------------------------------------------------------------------------------------
   --/ Creating a stored palette:

   --| The generic package `Palette_Storage` has one generic parameter, `Color_Type`, which
   --| specifies the colour system the package is to be based on.

   generic
      type Color_Type is private;

   package Palette_Storage is

      type Stored_Palette (Length: Palette_Length) is new Root_Palette_Type with private;

   private

      --/ Private part for palette storage:

      type Slot_Array is array (Color_Selector range <>) of Color_Type;

      type Stored_Palette (Length: Palette_Length) is new Root_Palette_Type with
         record
            Slots: Slot_Array(1..Length);
            Count: Color_Count range 0..Length := 0; -- number of engaged slots
         end record;

      --\

   end;

   --| A concrete (i.e. non-abstract) type, `Stored_Palette` (derived from
   --| `Root_Palette_Type`), is declared within this package. This type implements a stored
   --| palette for colour values of the specified colour system. The discriminant Length
   --| determines the length of the palette (which is therefore fixed).

   --| Fixed colours are never stored nor provided by palettes of this type (the `Fixed_Colors`
   --| function always returns zero). Objects of this type are always initialised with a zero
   --| number of engaged colours.

   --\
   --------------------------------------------------------------------------------------------
   --/ Stored palette for basic colors:

   package Basic_Color_Storage is new Palette_Storage(Basic_Color);

   --| `Basic_Color_Storage` is an instantiation of `Palette_Storage` for the type
   --| `Basic_Color`.

   --\
   --------------------------------------------------------------------------------------------
   --/ `Palette_Specialization` package:

   generic
      type Color_Type is private;

   package Palette_Specialization is

      --/ Specific palette type:

      type Specific_Palette is abstract new Root_Palette_Type with null record;

      --\

      --/ Specific palette operations:

      function Slot_Value (Selector: Color_Selector;
                           Palette:  Specific_Palette) return Color_Type is abstract;

      procedure Set_Slot (Palette:  in out Specific_Palette;
                          Selector: in     Color_Selector;
                          Color:    in     Color_Type) is abstract;

      procedure Engage (Palette:  in out Specific_Palette;
                        Color:    in     Color_Type) is abstract;

      procedure Engage (Palette:  in out Specific_Palette;
                        Color:    in     Color_Type;
                        Selector: out    Color_Selector) is abstract;

      --\

   end Palette_Specialization;

   --| The generic signature package `Palette_Specialization` provides a standard way to
   --| construct a new abstract class of palette (as a subclass of `Root_Palette_Type`) which
   --| can deal with a 'special color system', i.e. a color system other than RGBA.

   --| It should be noted that these palettes can also deal with the RGBA color system (because
   --| they are ultimately derived from `Root_Palette_Type`).

   --| The package takes one generic parameter, the type `Color_Type`, which represents values
   --| of the special color system in question.

   --| Within this package, the abstract type `Specific_Palette` is declared, which represents
   --| palettes which can deal with the special color system (as well as RGBA).

   --| Four primitive operations are added to this type, corresponding to the general palette
   --| operations which have an RGBA parameter or return type. For the specialized operations,
   --| the RGBA type (`Basic_Color`) is replaced by the special type (`Color_Type`). Note that
   --| these operations are _in addition_ to the RGBA versions: a specialized palette can deal
   --| with both the special colors _and_ RGBA colors.

   --\
   --------------------------------------------------------------------------------------------
   --/ Specialized stored palettes:

   generic
      with package Specialization is new Palette_Specialization(<>);
 
      with function To_Basic   (Color: Color_Type)  return Basic_Color is <>;
      with function From_Basic (Color: Basic_Color) return Color_Type  is <>;

   package Specialized_Storage is

      type Stored_Palette (Length: Palette_Length) is
         new Specialization.Specific_Palette with private;

   end;

   --| The generic package `Specialized_Storage` takes an instantiation of the
   --| `Palette_Specialization` package as one of its generic parameters, and declares the
   --| concrete type `Stored_Palette`. This package implements a stored palette for the special
   --| color type of the parameter package which is derived from this package's
   --| `Specific_Palette` type.

   --| This package has three generic parameters:

   --| * a package, `Specialization`, which is an instantiation of `Palette_Specialization`;

   --| * item the function `To_Basic`, which converts a value of the type
   --| `Specialization.Color_Type` into its equivalent RGBA representation;

   --| * the function From_Basic, which converts the other way.

   --| Whether the storage is done by double-storage (with conversion on demand), or by
   --| immediate conversion, or by some other method, is implementation defined.

   --\

-----------------------------------------------------------------------------------------------
private

   --/ Full declaration for Root_Palette_Type:

   type Root_Palette_Type is abstract tagged null record;

   --\

end Titian.Color;


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
