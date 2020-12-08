//------------------------------------------------------------------------------
//
//  I3D_Viewer - Model Viewer for Speed Haste models
//  Copyright (C) 2020 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  I3D Models
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/speed-game/
//------------------------------------------------------------------------------

unit i3d_model;

interface

uses
  Classes;
  
type
  TI3DModel = class(TObject)
  public
    procedure LoadFromFile(const fname: string);
    procedure Clear;
  end;

implementation

// ----------------------------- OBJECT3D.H ---------------------------
// For use with Watcom C.
// (C) Copyright 1995 by Jare & JCAB of Iguana.

// Definitive 3D object routines.

// ----------------------------- OBJECT3D.H ---------------------------
(*

    An object should be composed of:

        Detail? Or differently detailed objects shall be different objects?

        Vertices
        Normals
        Faces
            Normal
            Color
            Texture
            Vertices
                Normal
                Mapping

    Proposed structure: O3DF_ means disk data, O3DM_ means memory data.

    Skeleton file: were a value seems redundant, it will be used for
          consistency check, and won't appear in the binary version.
          This is especially true of the order numbers.

    Object "name"
    Vertices # Normals # Faces # FaceVertices # Materials #
    Flags 0x0000
    EndHeader
    Material # Color # Flags # Ambient # Diffuse # Reflected # Texture "tname"
     ...
    EndMaterials
    Vertex # X # Y # Z #
     ...
    EndVertices
    Normal # X # Y # Z #
     ...
    EndNormals
    Face # Vertices # Material # Flags # Array
        Vertex # Normal # TextureX # TextureY #
         ...
     ...
    EndFaces
*)

// -----------------------------------------
// Disk image of the object.

type
  vec3i_t = packed record
    x, y, z: integer;
  end;
  vec3i_p = ^vec3i_t;
  vec3i_tArray = packed array[0..$FFF] of vec3i_t;
  Pvec3i_tArray = ^vec3i_tArray;

  O3DF_TVertex = vec3i_t; // Pretty obvious.
  O3DF_TVertex_p = ^O3DF_TVertex;
  O3DF_TVertexArray = packed array[0..$FFF] of O3DF_TVertex;
  PO3DF_TVertexArray = ^O3DF_TVertexArray;
  O3DF_TNormal = vec3i_t;
  O3DF_TNormal_p = ^O3DF_TNormal;
  O3DF_TNormalArray = packed array[0..$FFF] of O3DF_TNormal;
  PO3DF_TNormalArray = ^O3DF_TNormalArray;

  O3DF_TFaceVertex = packed record
    nvert: smallint;  // 3D vertex index.
    nnorm: smallint;  // Normal vector at this vertex.
    tx, ty: integer;  // Texture values.
  end;
  O3DF_TFaceVertex_p = ^O3DF_TFaceVertex;
  O3DF_TFaceVertexArray = packed array[0..$FFF] of O3DF_TFaceVertex;
  PO3DF_TFaceVertexArray = ^O3DF_TFaceVertexArray;

  O3DF_TFaceHeader = packed record
    nVerts: word;
    material: smallint; // -1 => not visible.
    flags: LongWord;   // May indicate, for example, that it's a split.
    tox, toy, tsx, tsy, ta: intger;    // Texture data.
  end;
  O3DF_TFaceHeader_p = ^O3DF_TFaceHeader;
  O3DF_TFaceHeaderArray = packed array[0..$FFF] of O3DF_TFaceHeader;
  PO3DF_TFaceHeaderArray = ^O3DF_TFaceHeaderArray;

  O3DF_TFace = packed record
    h: O3DF_TFaceHeader;
  end;
  O3DF_TFace_p = ^O3DF_TFace;
  O3DF_TFaceArray = packed record[0..$FFF] of O3DF_TFace;
  PO3DF_TFaceArray = ^O3DF_TFaceArray;

  O3DF_TMaterial = packed record
    color: byte;
    flags: word;      // Semi-transparent? Translucid? etc.
    ambient: integer; // Lighting parameters.
    diffuse: integer;
    reflected: integer;
    texname: packed array[0..7] of char;  // Filename of the texture, or "" for flat.
  end;
  O3DF_TMaterial_p = ^O3DF_TMaterial;
  O3DF_TMaterialArray = packed array[0..$FFF] of O3DF_TMaterial;
  PO3DF_TMaterialArray = ^O3DF_TMaterialArray;

  O3DF_TObject = packed record
    nVerts: word;        // Are rotated and translated.
    nNormals: word;      // Are rotated, but not translated.
    nFaces: word;
    nFaceVerts: word;
    nMaterials: word;
        // Object loader can determine the amount of mem to alloc in a
        // single block, from the nXXX values above -> Less heap overhead.
    flags: word;
    scx, scy, scz: integer;     // Scale factors for the application to handle.
                                // Recommended format is 16.16.
                                // But note that they default to 0.
    dcx, dcy, dcz: integer;     // Center for the application to handle.
                                // Recommended format is: same as vertices
                                // *before* scaling.
  end;
  O3DF_TObject_p = ^O3DF_TObject;
  O3DF_TObjectArray = packed array[0..$FFF] of O3DF_TObject;
  PO3DF_TObjectArray = ^O3DF_TObjectArray;

// -----------------------------------------
// Memory image of the object.

typedef struct {
    byte    color;
    word    flags;          // Semi-transparent? Translucid? etc.
    sint32  ambient;        // Lighting parameters.
    sint32  diffuse;
    sint32  reflected;
    char    texname[8];     // Filename of the texture, or "" for flat.
    byte    *texture;       // Pointer to loaded texture mem.
} O3DM_TMaterial, *O3DM_PMaterial;

typedef struct {
    sint32  x, y, z;            // Pretty obvious.
    sint32  rx, ry, rz;
    sint32  px, py;
    sint32  l;                  // Calculated.
} O3DM_TVertex, *O3DM_PVertex;

typedef struct {
    sint32  x, y, z;            // Pretty obvious.
    sint32  rx, ry, rz;
    sint32  l;                  // Calculated
} O3DM_TNormal, *O3DM_PNormal;

typedef struct {
    O3DM_PVertex vert;          // 3D vertex index.
    O3DM_PNormal normal;        // Normal vector at this vertex.
    sint32 l;                   // Calculated
    sint32 tx, ty;              // Texture values.
} O3DM_TFaceVertex, *O3DM_PFaceVertex;

struct O3DM_SFace;          // Forward declaration.

typedef struct {
    bool            visible;    // Calculated.
    word            nVerts;
    dword           flags;      // May indicate, for example, that it's a split.
    O3DM_PMaterial  material;   // NULL => not to be drawn.
    sint32      tox, toy, tsx, tsy, ta;    // Texture data.
    struct O3DM_SFace *back, *front; // BSP links, or doubly linked list
                                           // of regular faces in the BSP leaf.
    sint32 depth;               // Calculated.
    struct O3DM_SFace *next;
} O3DM_TFaceHeader, *O3DM_PFaceHeader;

typedef struct O3DM_SFace {
    O3DM_TFaceHeader h;
    O3DM_TFaceVertex verts[1];
} O3DM_TFace, *O3DM_PFace;

typedef struct {
    word nVerts;        // Are rotated and translated.
    word nNormals;      // Are rotated, but not translated.
    word nFaces;
    word nFaceVerts;
    word nMaterials;
        // Object loader can determine the amount of mem to alloc in a
        // single block, from the nXXX values above -> Less heap overhead.
    word flags;
    O3DM_PVertex   verts;
    O3DM_PNormal   normals;
    O3DM_PFace     faces;
    O3DM_PMaterial materials;

    R3D_PPosVector   pos;
    R3D_PAngles      rot;
    sint32 scx, scy, scz;       // Scale factors for the application to handle.
                                // Recommended format is 16.16.
                                // But note that they default to 0.
    sint32 dcx, dcy, dcz;       // Center for the application to handle.
                                // Recommended format is: same as vertices
                                // *before* scaling.
} O3DM_TObject, *O3DM_PObject;

const
  // Material flags.
  O3DMF_NOSHADE = $0001;     // Don't apply any lightning.
  O3DMF_TRANS   = $0002;     // Translucent.
  O3DMF_HOLES   = $0004;     // Color 0 in the texture is transparent.
  O3DMF_256     = $0008;     // 256 wide bitmap

  // Face flags.
  O3DFF_FLAT    = $0001;     // Face is flat shaded (1st vertex' normal).
  O3DFF_NORDER  = $0002;     // Face should be rendered prior to the rest.
  O3DFF_VISIBLE = $0004;     // Should be though of as visible always.



PUBLIC int O3DM_MaxDetail;
PUBLIC int O3DM_OrderByZ;

const
  O3DD_FLAT = 0;
  O3DD_GOURAUD = 1;
  O3DD_TEXTURED = 2;
  O3DD_TEXLIGHT = 3;
  O3DD_TEXGOURAUD = 4;
  O3DD_MAXDETAIL = 5;

PUBLIC O3DM_PObject O3DM_LoadObject(const char *fname);
PUBLIC bool         O3DM_SaveObject(const char *fname, O3DM_PObject obj);

    // Delete all memory taken by an object.
PUBLIC void O3DM_DeleteObject(O3DM_PObject obj);

    // Make a copy of an object, one that can be modified easily.
PUBLIC O3DM_PObject O3DM_DupObj(O3DM_PObject obj);

#define SIZEFACE(f) (sizeof((f)->h) + sizeof((f)->verts[0])*(f)->h.nVerts)

#define O3DM_RotateObjVerts(obj,m)   (R3D_Rot3DVector(&(obj)->verts[0].rx,(m),&(obj)->verts[0].x,(obj)->nVerts,sizeof((obj)->verts[0])))
#define O3DM_RotateObjNormals(obj,m) (R3D_Rot3DVector(&(obj)->normals[0].rx,(m), &(obj)->normals[0].x,(obj)->nNormals,sizeof((obj)->normals[0])))
#define O3DM_TranslateObj(obj,p)     (R3D_Add3DVector(&(obj)->verts[0].rx,(p),(obj)->nVerts,sizeof((obj)->verts[0])))
#define O3DM_ProjectObject(obj)      (R3D_Project3D(&(obj)->verts[0].px,&(obj)->verts[0].rx,(obj)->nVerts, sizeof(obj->verts[0]), sizeof((obj)->verts[0])))

PUBLIC O3DM_PFace O3DM_OrderFaces(O3DM_PObject obj);
PUBLIC void O3DM_ParseVisibility (O3DM_PObject obj);

PUBLIC void O3DM_CalcLights      (O3DM_PObject obj, dword ang);
PUBLIC void O3DM_DrawFace        (O3DM_PFace pl);
PUBLIC void O3DM_Draw            (O3DM_PObject obj);

#endif

// ----------------------------- OBJECT3D.H ---------------------------

procedure TI3DModel.LoadFromFile(const fname: string);
begin
end;

procedure TI3DModel.Clear;
begin
end;

end.
