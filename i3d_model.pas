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

procedure TI3DModel.LoadFromFile(const fname: string);
begin
end;

procedure TI3DModel.Clear;
begin
end;

end.
