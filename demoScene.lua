-- title:  Ara-ara 3D engine
-- author: HanamileH
-- desc:   version 1.0 beta
-- script: lua

-- github: https://github.com/hanamileh-DEV/Ara_ara_engine

--we kindly ask you to specify the name of this engine if you use it for your projects

--[[
license:

Copyright 2024 HanamileH
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
]]

local vec3, matrix, Ara, obj

-- Functions that may be useful
local function clamp(val, minVal, maxVal) return val<minVal and minVal or val>maxVal and maxVal or val end
local function min_abs(a,b) return (math.abs(a) < math.abs(b)) and a or b end
local function max_abs(a,b) return (math.abs(a) > math.abs(b)) and a or b end
local function sign(val) return (val > 0) and 1 or (val < 0) and -1 or 0 end
local function Rf(min_v,max_v) --random float
	if not max_v then return Rf(-min_v,min_v) end
	return math.random()*(max_v-min_v)+min_v
end
local function lerp(a,b,coef) return a + (b - a)*coef end
local function mapVal(val,a_min,a_max,b_min,b_max) return (val-a_min)/(a_max-a_min)*(b_max-b_min) + b_min end
local function step(val, x) return (x > val) and 1 or 0 end
local function smoothstep(val_a,val_b,x)
	x=clamp((x-val_a)/(val_b-val_a),0,1)
	return x*x*(3-2*x)
end

---- Vector library ----
local vecMt = {}
vecMt.__index = vecMt

do
	function vecMt.__add(a,b)
		if type(a) == "number" then a = vec3(a) end
		if type(b) == "number" then b = vec3(b) end
		return vec3(a.x + b.x, a.y + b.y, a.z + b.z)
	end

	function vecMt.__sub(a,b)
		if type(a) == "number" then a = vec3(a) end
		if type(b) == "number" then b = vec3(b) end
		return vec3(a.x - b.x, a.y - b.y, a.z - b.z)
	end

	function vecMt.__mul(a,b)
		if type(a) == "number" then a = vec3(a) end
		if type(b) == "number" then b = vec3(b) end

		if a[1] then a, b = b, a end
		-- multiplying by the matrix
		if b[1] then
			local x = b[1][1]*a.x + b[1][2]*a.y + b[1][3]*a.z + b[1][4]
			local y = b[2][1]*a.x + b[2][2]*a.y + b[2][3]*a.z + b[2][4]
			local z = b[3][1]*a.x + b[3][2]*a.y + b[3][3]*a.z + b[3][4]
			local w = b[4][1]*a.x + b[4][2]*a.y + b[4][3]*a.z + b[4][4]

			return vec3(x/w, y/w, z/w)
		else
			return vec3(a.x * b.x, a.y * b.y, a.z * b.z)
		end
	end

	function vecMt.__div(a,b)
		if type(a) == "number" then a = vec3(a) end
		if type(b) == "number" then b = vec3(b) end
		return vec3(a.x / b.x, a.y / b.y, a.z / b.z)
	end
	
	function vecMt.__mod(a,b)
		if type(a) == "number" then a = vec3(a) end
		if type(b) == "number" then b = vec3(b) end
		return vec3(a.x % b.x, a.y % b.y, a.z % b.z)
	end

	function vecMt.__idiv(a,b)
		if type(a) == "number" then a = vec3(a) end
		if type(b) == "number" then b = vec3(b) end
		return vec3(a.x // b.x, a.y // b.y, a.z // b.z)
	end

	function vecMt.__tostring(self)
		return string.format("[%.1f;%.1f;%.1f]",self.x,self.y,self.z)
	end

	function vecMt.__cocnat(a,b)
		return tostring(a)..tostring(b)
	end

	function vecMt:length()
		return math.sqrt(self.x*self.x + self.y*self.y + self.z*self.z)
	end
	
	function vecMt:normalize()
		local len = math.sqrt(self.x*self.x + self.y*self.y + self.z*self.z)

		if len > 0 then
			return vec3(self.x / len, self.y / len, self.z / len)
		else
			return vec3()
		end
	end
end

function vec3(vx,vy,vz)
	vx = tonumber(vx or 0)
	vy = tonumber(vy or vx or 0)
	vz = tonumber(vz or vx or 0)

	local vector = {x = vx, y = vy, z = vz}

	return setmetatable(vector,vecMt)
end

---- Matrix library ----
local matMt = {} -- matrix metatable

function matMt.__mul(m1, m2)
	-- This code may seem quite terrible (it is)
	-- But its main goal is to get the maximum
	-- performance by reducing the quantity
	-- Requests to tables, as well as no cycles.
	-- In fact, this is a standard algorithm
	-- for multiplying matrices.

	local a1,a2,a3,a4 = m1[1],m1[2],m1[3],m1[4]
	local b1,b2,b3,b4 = m2[1],m2[2],m2[3],m2[4]

	local a11,a12,a13,a14 = a1[1],a1[2],a1[3],a1[4]
	local a21,a22,a23,a24 = a2[1],a2[2],a2[3],a2[4]
	local a31,a32,a33,a34 = a3[1],a3[2],a3[3],a3[4]
	local a41,a42,a43,a44 = a4[1],a4[2],a4[3],a4[4]
	
	local b11,b12,b13,b14 = b1[1],b1[2],b1[3],b1[4]
	local b21,b22,b23,b24 = b2[1],b2[2],b2[3],b2[4]
	local b31,b32,b33,b34 = b3[1],b3[2],b3[3],b3[4]
	local b41,b42,b43,b44 = b4[1],b4[2],b4[3],b4[4]

	local result = {
		{a11*b11+a12*b21+a13*b31+a14*b41,a11*b12+a12*b22+a13*b32+a14*b42,a11*b13+a12*b23+a13*b33+a14*b43,a11*b14+a12*b24+a13*b34+a14*b44},
		{a21*b11+a22*b21+a23*b31+a24*b41,a21*b12+a22*b22+a23*b32+a24*b42,a21*b13+a22*b23+a23*b33+a24*b43,a21*b14+a22*b24+a23*b34+a24*b44},
		{a31*b11+a32*b21+a33*b31+a34*b41,a31*b12+a32*b22+a33*b32+a34*b42,a31*b13+a32*b23+a33*b33+a34*b43,a31*b14+a32*b24+a33*b34+a34*b44},
		{a41*b11+a42*b21+a43*b31+a44*b41,a41*b12+a42*b22+a43*b32+a44*b42,a41*b13+a42*b23+a43*b33+a44*b43,a41*b14+a42*b24+a43*b34+a44*b44}
	}

	return matrix.new(result)
end
-- I'm not sure if anything other
-- than matrix multiplication is needed,
-- but if so, let me know!

matrix = {
	-- Creating a new matrix (specified in the form of a table or a default matrix)
	new = function(mat)
		mat = mat or {
			{1, 0, 0, 0},
			{0, 1, 0, 0},
			{0, 0, 1, 0},
			{0, 0, 0, 1},
		}

		setmetatable(mat, matMt)

		return mat
	end,

	-- Rotation matrices along each axis
	rotationX = function(angle)
		local cosA = math.cos(angle)
		local sinA = math.sin(angle)
		return matrix.new({
			{1, 0, 0, 0},
			{0, cosA,-sinA, 0},
			{0, sinA, cosA, 0},
			{0, 0, 0, 1}
		})
	end,

	rotationY = function(angle)
		local cosA = math.cos(angle)
		local sinA = math.sin(angle)
		return matrix.new({
			{cosA, 0, sinA, 0},
			{0, 1, 0, 0},
			{-sinA, 0, cosA, 0},
			{0, 0, 0, 1}
		})
	end,

	rotationZ = function(angle)
		local cosA = math.cos(angle)
		local sinA = math.sin(angle)
		return matrix.new({
			{cosA,-sinA, 0, 0},
			{sinA, cosA, 0, 0},
			{0, 0, 1, 0},
			{0, 0, 0, 1}
		})
	end,

	-- rotation using quatternions
	quatRotation = function(x, y, z, w)
		local len = math.sqrt(x*x + y*y + z*z + w*w)

		assert(len>0,"The quaternion cannot be zero")

		x,y,z,w = x/len, y/len, z/len, w/len

		local xx, yy, zz = x*x, y*y, z*z
		local xy, xz, yz = x*y, x*z, y*z
		local wx, wy, wz = w*x, w*y, w*z

		return matrix.new({
			{1-2*(yy+zz), 2*(xy-wz), 2*(xz+wy), 0},
			{2*(xy+wz), 1-2*(xx+zz), 2*(yz-wx), 0},
			{2*(xz-wy), 2*(yz+wx), 1-2*(xx+yy), 0},
			{0, 0, 0, 1}
		})
	end,

	-- Translation matrix (an argument can be either a vector or 3 numbers)
	translation = function(posX, posY, posZ)
		if type(posX) ~= "number" then
			posX, posY, posZ = posX.x, posX.y, posX.z
		end

		return matrix.new({
			{1, 0, 0, posX},
			{0, 1, 0, posY},
			{0, 0, 1, posZ},
			{0, 0, 0, 1}
		})
	end,

	-- Scaling matrix
	-- Scaling scales in each direction
	-- if the argument is a vector, or 3 numbers,
	-- otherwise if it is 1 number,
	-- it scales equally in all directions
	scale = function(scaleX, scaleY, scaleZ)
		if type(scaleX) ~= "number" then
			scaleX, scaleY, scaleZ = scaleX.x, scaleX.y, scaleX.z
		elseif not scaleY then
			scaleY, scaleZ = scaleX, scaleX
		end

		return matrix.new({
			{scaleX,0,0,0},
			{0,scaleY,0,0},
			{0,0,scaleZ,0},
			{0,0,0,1},
		})
	end,

	-- Projection matrix
	projection = function()
		local tg = math.tan(math.rad(Ara.camera.fov/2))

		return matrix.new({
			{1/tg,0   ,0,0},
			{0   ,1/tg,0,0},
			{0   ,0   ,1,0},
			{0   ,0   ,1,0}
		})
	end,

	-- Ready-made camera matrix
	camera = function(pitch, yaw, roll, pos)
		pitch = pitch or Ara.camera.pitch
		yaw   = yaw   or Ara.camera.yaw
		roll  = roll  or Ara.camera.roll
		pos   = pos   or Ara.camera.pos

		local rotationX = matrix.rotationX(pitch)
		local rotationY = matrix.rotationY(yaw)
		local rotationZ = matrix.rotationZ(roll)

		local view = rotationZ * rotationX * rotationY

		local translation = matrix.translation(0-pos)
		local projection = matrix.projection()

		return projection * view * translation
	end,
}

---- Engine configuration ----
Ara = {
	version = "1.0",

	camera = {
		-- The vertical viewing angle of the camera
		fov = 120,

		-- The near cutting plane (must be strictly greater than 0)
		near = 0.1,

		-- Rotation and position of the camera
		pitch = 0,
		yaw = 0,
		roll = 0,

		pos = vec3(),
	},
	
	-- Default values
	wireframeColorDefault = 0,
	pointsColorDefault = 9,
	pointsRadiusDefault = 0,

	-- The final matrix of projection-View
	matrix = matrix.new(),
}

---- functions ----
function Ara.drawSky(skyColor, groundColor, pitch)
	pitch = pitch or Ara.camera.pitch

	-- Sky
	cls(skyColor)

	-- Ground
	local groundHeight = 68 + 120 * math.tan(pitch) / math.tan(math.rad(Ara.camera.fov/2))

	groundHeight = clamp(groundHeight, 0, 136)

	rect(0, groundHeight, 240, 137-groundHeight, groundColor)
end

-- applying a matrix to all vertices of a table
-- (note that this function does not use the world matrix by default)
function Ara.update(model, mat)
	local vertices = model.v

	local result = {}

	-- Transform each point with a matrix
	for i = 1, #vertices do
		local v = vertices[i]

		local x = mat[1][1]*v[1] + mat[1][2]*v[2] + mat[1][3]*v[3] + mat[1][4]
		local y = mat[2][1]*v[1] + mat[2][2]*v[2] + mat[2][3]*v[3] + mat[2][4]
		local z = mat[3][1]*v[1] + mat[3][2]*v[2] + mat[3][3]*v[3] + mat[3][4]
		local w = mat[4][1]*v[1] + mat[4][2]*v[2] + mat[4][3]*v[3] + mat[4][4]

		result[i] = {
			x= x/w*120 + 120,
			y=-y/w*120 + 68,
			z= z/w,
			w= w,
			-- values without W-division
			x2=x, y2=-y, z2=z
		}
	end

	return result
end

-- drawing vertices (points)
function Ara.drawPoints(vertices, color, radius)
	color = color or Ara.pointsColorDefault
	radius = radius or Ara.pointsRadiusDefault

	for i = 1, #vertices do
		local v = vertices[i]

		if v.z2 > Ara.camera.near then
			pix(v.x, v.y, color)
			if radius then circb(v.x, v.y, radius, color) end
		end
	end
end

-- Drawing object surfaces
function Ara.drawPolys(vertices, faces)
	local near = Ara.camera.near

	for i = 1, #faces do
		local poly = faces[i]

		if poly.f == 0 then goto skip end

		local v1 = vertices[poly[1]]
		local v2 = vertices[poly[2]]
		local v3 = vertices[poly[3]]

		local uv = poly.uv

		local triPoints = (v2.x-v1.x) * (v3.y-v1.y) - (v3.x-v1.x) * (v2.y-v1.y) > 0
		local drawTri = (poly.f==3) or (triPoints == (poly.f==2))

		local z = {v1.z2>near, v2.z2>near, v3.z2>near}

		if poly.f < 3 then
			drawTri = drawTri ~= (v1.z2>0) ~= (v2.z2>0) ~= (v3.z2>0)
		end

		if not drawTri then goto skip end

		if z[1] and z[2] and z[3] then
			ttri(v1.x, v1.y,
				v2.x, v2.y,
				v3.x, v3.y,
				uv[1][1], uv[1][2],
				uv[2][1], uv[2][2],
				uv[3][1], uv[3][2], 0, uv[4],
				v1.z2, v2.z2, v3.z2
			)
		elseif z[1] or z[2] or z[3] then
			-- Z-clipping
			--(It's a pain, but the pain works)
			v1.i = 1
			v2.i = 2
			v3.i = 3

			local v = {v1, v2, v3}

			table.sort(v, function(a,b) return a.z2<b.z2 end)

			local i1 = v[1].i
			local i2 = v[2].i
			local i3 = v[3].i

			if v[2].z2>near then
				local v4, v5 = {},{}
				--------
				local t = (near - v[1].z2) / (v[3].z2 - v[1].z2)

				v4.x2 = lerp(v[1].x2,v[3].x2, t)
				v4.y2 = lerp(v[1].y2,v[3].y2, t)

				v4.w = near

				v4.x = v4.x2/v4.w*120 + 120
				v4.y = v4.y2/v4.w*120 + 68

				v4.uv = {
					lerp(uv[i1][1],uv[i3][1], t),
					lerp(uv[i1][2],uv[i3][2], t)
				}
				--------
				t = (near - v[1].z2) / (v[2].z2 - v[1].z2)

				v5.x2 = lerp(v[1].x2,v[2].x2, t)
				v5.y2 = lerp(v[1].y2,v[2].y2, t)
				v5.w = near

				v5.x = v5.x2/v5.w*120 + 120
				v5.y = v5.y2/v5.w*120 + 68

				v5.uv = {
					lerp(uv[i1][1],uv[i2][1], t),
					lerp(uv[i1][2],uv[i2][2], t)
				}
				--------
				ttri(v[2].x,v[2].y,v4.x,  v4.y,  v5.x,v5.y,uv[i2][1],uv[i2][2],v4.uv[1], v4.uv[2], v5.uv[1],v5.uv[2],0,uv[4],v[2].z2,near,   near)
				ttri(v[3].x,v[3].y,v[2].x,v[2].y,v4.x,v4.y,uv[i3][1],uv[i3][2],uv[i2][1],uv[i2][2],v4.uv[1],v4.uv[2],0,uv[4],v[3].z2,v[2].z2,near)
			else
				local v4, v5 = {},{}
				--------
				local t = (near - v[1].z2) / (v[3].z2 - v[1].z2)

				v4.x2 = lerp(v[1].x2, v[3].x2, t)
				v4.y2 = lerp(v[1].y2, v[3].y2, t)
				v4.w = near

				v4.x = v4.x2/v4.w*120 + 120
				v4.y = v4.y2/v4.w*120 + 68
				
				v4.uv = {
					lerp(uv[i1][1],uv[i3][1], t),
					lerp(uv[i1][2],uv[i3][2], t)
				}
				--------
				t = (near - v[3].z2) / (v[2].z2 - v[3].z2)

				v5.x2 = lerp(v[3].x2, v[2].x2, t)
				v5.y2 = lerp(v[3].y2, v[2].y2, t)
				v5.w = near

				v5.x = v5.x2/v5.w*120 + 120
				v5.y = v5.y2/v5.w*120 + 68

				v5.uv = {
					lerp(uv[i3][1],uv[i2][1], t),
					lerp(uv[i3][2],uv[i2][2], t)
				}
				--------
				ttri(v[3].x,v[3].y,v4.x,v4.y,v5.x,v5.y,uv[i3][1],uv[i3][2],v4.uv[1],v4.uv[2],v5.uv[1],v5.uv[2],0,uv[4],v[3].z2,near,near)
			end
		end
		::skip::
	end
end

-- Drawing a wireframe object
function Ara.drawWireframe(vertices, faces, color)
	color = color or Ara.wireframeColorDefault
	local near = Ara.camera.near

	for i = 1, #faces do
		local poly = faces[i]

		local v1 = vertices[poly[1]]
		local v2 = vertices[poly[2]]
		local v3 = vertices[poly[3]]

		local z = {v1.z2>near, v2.z2>near, v3.z2>near}

		if z[1] or z[2] or z[3] then
			if z[1] and z[2] and z[3] then
				trib(v1.x,v1.y, v2.x,v2.y, v3.x,v3.y, color)
			else
				-- Z-clipping
				v1.i = 1
				v2.i = 2
				v3.i = 3

				local v = {v1, v2, v3}

				table.sort(v, function(a,b) return a.z2<b.z2 end)

				if v[2].z2>near then
					local v4, v5 = {},{}
					--------
					local t = (near - v[1].z2) / (v[3].z2 - v[1].z2)

					v4.x2 = lerp(v[1].x2,v[3].x2, t)
					v4.y2 = lerp(v[1].y2,v[3].y2, t)
					v4.w = near

					v4.x = v4.x2/v4.w*120 + 120
					v4.y = v4.y2/v4.w*120 + 68
					--------
					t = (near - v[1].z2) / (v[2].z2 - v[1].z2)

					v5.x2 = lerp(v[1].x2,v[2].x2, t)
					v5.y2 = lerp(v[1].y2,v[2].y2, t)
					v5.w = near

					v5.x = v5.x2/v5.w*120 + 120
					v5.y = v5.y2/v5.w*120 + 68
					--------
					line(v[2].x,v[2].y, v5.x,v5.y,color)
					line(v[3].x,v[3].y, v4.x,v4.y,color)
					line(v[2].x,v[2].y,v[3].x,v[3].y,color)
					line(v4.x,v4.y, v5.x,v5.y,color)
				else
					
					local v4, v5 = {},{}
					--------
					local t = (near - v[1].z2) / (v[3].z2 - v[1].z2)

					v4.x2 = lerp(v[1].x2, v[3].x2, t)
					v4.y2 = lerp(v[1].y2, v[3].y2, t)
					v4.w = near

					v4.x = v4.x2/v4.w*120 + 120
					v4.y = v4.y2/v4.w*120 + 68
					--------
					t = (near - v[3].z2) / (v[2].z2 - v[3].z2)

					v5.x2 = lerp(v[3].x2, v[2].x2, t)
					v5.y2 = lerp(v[3].y2, v[2].y2, t)
					v5.w = near

					v5.x = v5.x2/v5.w*120 + 120
					v5.y = v5.y2/v5.w*120 + 68
					--------
					line(v[3].x,v[3].y, v4.x,v4.y,color)
					line(v[3].x,v[3].y, v5.x,v5.y,color)
					line(v4.x,v4.y, v5.x,v5.y,color)
				end
			end
		end
	end
end

---- Object library ----
obj = {}

function obj.new(model)
	local object = {
		model = model,
		matrix = matrix.new(),
	}

	-- Transformation methods
	function object:pos(...) self.matrix = matrix.translation(...) * self.matrix end
	function object:scale(...) self.matrix = matrix.scale(...) * self.matrix end
	function object:rotateX(angle) self.matrix = matrix.rotationX(angle) * self.matrix end
	function object:rotateY(angle) self.matrix = matrix.rotationY(angle) * self.matrix end
	function object:rotateZ(angle) self.matrix = matrix.rotationZ(angle) * self.matrix end
	
	function object:initMat() self.matrix = matrix.new() end
	function object:addMat(mat) self.matrix = mat * self.matrix end

	-- Rendering an object
	function object:draw(faces, wireframe, points, wireframeColor, pointsColor, pointsRadius)
		if faces == nil then faces = true end

		local mat = Ara.matrix * self.matrix

		local processed = Ara.update(self.model, mat)

		-- Rendering faces with textures
		if faces then
			Ara.drawPolys(processed, self.model.f)
		end

		-- Rendering wireframe
		if wireframe then
			Ara.drawWireframe(processed, self.model.f, wireframeColor)
		end

		-- Rendering vertices
		if points then
			Ara.drawPoints(processed, pointsColor, pointsRadius)
		end
	end

	function object:drawPos(...)
		local args = {...}
		local pos
		if type(args[1]) ~= "table" then
			pos = vec3(args[1], args[2], args[3])

			-- Removing the first elements from the arguments
			table.remove(args,1)
			table.remove(args,1)
			table.remove(args,1)
		else
			pos = args[1]
			
			table.remove(args,1)
		end
		-- Saving the previous matrix
		local mat = self.matrix
		-- Drawing with a new matrix
		self.matrix = self.matrix * matrix.translation(pos)
		self:draw(table.unpack(args))
		-- Returning the previous matrix
		self.matrix = mat
	end

	return object
end

-- Objects --
local treeModel, benchModel, interactModel

do
	treeModel = {
		v={{-19,0,-1},{45,0,43},{37,0,-39},{23,606,-11},{-2,509,-5},{3,265,1},{-22,122,2},{44,130,-36},{48,259,-9},{36,516,-9},{25,524,23},{44,392,20},{35,256,35},{39,127,32},{52,399,-21},{17,397,-4},{138,484,-18},{-65,632,187},{95,407,-36},{67,405,60},{-218,577,-88},{13,394,24},{-242,582,-15},{31,485,-194},{183,598,59},{168,656,24},{2,645,190},{106,697,93},{-53,659,128},{-238,605,-74},{-204,624,-100},{141,681,20},{202,593,4},{-194,593,-107},{-211,555,-19},{-66,410,85},{82,404,16},{122,435,39},{55,469,137},{-44,586,-162},{116,607,-108},{108,624,63},{-56,604,129},{-12,681,-22},{-37,663,64},{-101,643,-37},{-39,650,-125},{50,624,123},{25,622,-147},{-105,595,-98},{92,548,114},{89,546,-158},{171,560,-82},{-106,553,-160},{-22,540,-197},{-171,544,31},{-159,541,-89},{-66,448,121},{90,480,81},{20,415,59},{104,475,-95},{-65,417,29},{-101,441,-54},{19,662,105},{25,660,34},{-19,621,-134},{45,633,-96},{-88,599,-58},{-62,648,-83},{-121,577,85},{-160,571,57},{77,590,123},{33,554,130},{27,595,141},{168,609,-61},{149,569,-38},{6,584,-170},{36,557,-181},{69,581,-141},{-130,561,-104},{-126,551,-135},{-89,572,-139},{-36,490,131},{-54,551,125},{97,536,54},{88,507,-121},{141,526,-106},{125,559,-124},{-86,497,-168},{-52,501,-172},{-65,544,-191},{-157,507,11},{-140,536,-44},{-24,420,88},{78,461,26},{52,445,79},{44,420,-65},{-82,431,-11},{-138,457,-41},{-84,459,-93},{-47,441,-57},{-24,449,-125},{18,446,-154},{-47,447,-17},{-112,462,-136},{-115,474,16},{-22,404,23},{119,456,-27},{58,486,-151},{102,474,46},{-5,446,135},{-145,537,48},{-116,510,-157},{149,588,-98},{128,527,1},{-87,565,121},{28,514,153},{-145,588,-77},{79,637,99},{-28,659,-81},{33,658,-38},{24,663,1},{-30,665,19},{-33,630,88},{79,653,45},{-67,654,-41},{1,617,129},{133,629,6},{71,615,-127},{-60,581,-129},{-32,583,143},{-129,519,89},{108,604,18},{86,508,126},{161,526,-50},{-1,513,-191},{-184,574,0},{-86,486,98},{117,482,-50},{-78,466,40},{-29,471,-177},{-135,453,-102},{19,413,-42},{61,645,74},{-70,656,8},{-115,623,1},{-93,643,50},{-11,377,-17},{-186,482,-210},{28,458,-259},{242,466,21},{-38,592,-215},{-65,632,187},{-6,731,-25},{38,396,-140},{-109,398,-104},{-52,442,-187},{-206,412,-59},{-104,368,5},{95,407,-36},{192,472,-127},{67,405,60},{138,467,120},{-15,406,183},{-242,531,44},{-135,564,-218},{220,559,-96},{179,591,-233},{193,547,34},{-139,556,165},{26,548,177},{22,676,-176},{203,632,-34},{52,664,189},{-167,644,86},{-1,697,-147},{-121,693,-50},{98,712,-96},{56,749,93},{-40,712,93},{9,372,-87},{-157,441,-139},{-20,462,-229},{-199,459,-123},{-114,438,56},{54,399,-30},{209,472,-60},{13,394,24},{-83,473,136},{-208,515,-150},{-242,582,-15},{-1,511,-257},{-90,594,-229},{226,513,-46},{179,611,-182},{183,598,59},{-35,605,191},{96,641,-163},{2,645,190},{-34,649,-172},{-66,721,-27},{127,674,-108},{106,697,93},{31,430,-191},{-59,388,-67},{-112,468,-196},{-173,463,32},{174,450,-5},{119,467,-197},{74,431,141},{186,460,48},{31,437,188},{-192,520,66},{-143,523,-213},{149,621,123},{210,510,30},{-100,596,173},{51,499,208},{-204,624,-100},{-2,634,-201},{187,637,-82},{-3,715,-91},{45,705,-57},{36,743,19},{-33,730,26},{-78,706,23},{-154,669,28},{-98,677,91},{13,721,96},{61,701,132},{84,722,-7},{141,681,20},{139,670,-67},{31,710,-115},{67,692,-140},{-68,702,-101},{-99,652,-152},{-154,656,-87},{-199,589,65},{-177,547,100},{-153,599,126},{38,606,187},{202,593,4},{202,568,-29},{208,604,-64},{98,632,-207},{70,577,-262},{-14,618,-234},{-158,587,-178},{-193,567,-165},{-194,593,-107},{-66,541,183},{-71,481,170},{-3,484,183},{170,559,101},{140,518,142},{171,502,78},{195,569,-162},{215,512,-118},{177,517,-174},{-92,560,-255},{-97,506,-207},{-211,555,-19},{-213,498,-83},{-212,479,-19},{-66,410,85},{21,411,129},{82,404,16},{122,435,39},{143,433,-82},{67,398,-87},{123,439,-143},{-155,407,-6},{-108,388,-41},{-8,413,-167},{-43,401,-119},{-80,422,-140},{-115,499,60},{-175,620,-6},{-129,519,89}},
		f={{14,1,2,uv={{0,4},{2,2},{2,4}},f=1},{12,6,13,uv={{0,4},{2,2},{2,4}},f=1},{12,5,16,uv={{0,4},{2,2},{2,4}},f=1},{7,3,1,uv={{0,4},{2,2},{2,4}},f=1},{16,9,6,uv={{0,4},{2,2},{2,4}},f=1},{16,10,15,uv={{0,4},{2,2},{2,4}},f=1},{14,7,1,uv={{0,4},{2,2},{2,4}},f=1},{12,16,6,uv={{0,4},{2,2},{2,4}},f=1},{12,11,5,uv={{0,4},{2,2},{2,4}},f=1},{7,8,3,uv={{0,4},{2,2},{2,4}},f=1},{16,15,9,uv={{0,4},{2,2},{2,4}},f=1},{16,5,10,uv={{0,4},{2,2},{2,4}},f=1},{13,6,7,uv={{0,4},{2,2},{2,4}},f=1},{3,8,14,uv={{0,4},{2,2},{2,4}},f=1},{9,13,14,uv={{0,4},{2,2},{2,4}},f=1},{9,15,12,uv={{0,4},{2,2},{2,4}},f=1},{10,11,12,uv={{0,4},{2,2},{2,4}},f=1},{6,8,7,uv={{0,4},{2,2},{2,4}},f=1},{10,12,15,uv={{0,4},{2,2},{2,4}},f=1},{9,14,8,uv={{0,4},{2,2},{2,4}},f=1},{13,7,14,uv={{0,4},{2,2},{2,4}},f=1},{6,9,8,uv={{0,4},{2,2},{2,4}},f=1},{11,4,5,uv={{0,4},{2,2},{2,4}},f=1},{3,14,2,uv={{0,4},{2,2},{2,4}},f=1},{9,12,13,uv={{0,4},{2,2},{2,4}},f=1},{10,4,11,uv={{0,4},{2,2},{2,4}},f=1},{5,4,10,uv={{0,4},{2,2},{2,4}},f=1},{98,104,107,uv={{2,2},{2,4},{4,4}},f=3},{18,27,29,uv={{2,2},{2,4},{4,4}},f=3},{100,105,141,uv={{2,2},{2,4},{4,4}},f=3},{101,63,100,uv={{2,2},{2,4},{4,4}},f=3},{102,101,100,uv={{2,2},{2,4},{4,4}},f=3},{102,100,141,uv={{2,2},{2,4},{4,4}},f=3},{103,102,141,uv={{2,2},{2,4},{4,4}},f=3},{118,146,46,uv={{2,2},{2,4},{4,4}},f=3},{21,30,34,uv={{2,2},{2,4},{4,4}},f=3},{124,64,45,uv={{2,2},{2,4},{4,4}},f=3},{129,49,67,uv={{2,2},{2,4},{4,4}},f=3},{134,51,73,uv={{2,2},{2,4},{4,4}},f=3},{135,53,76,uv={{2,2},{2,4},{4,4}},f=3},{115,135,76,uv={{2,2},{2,4},{4,4}},f=3},{113,81,54,uv={{2,2},{2,4},{4,4}},f=3},{37,19,38,uv={{2,2},{2,4},{4,4}},f=3},{135,139,87,uv={{2,2},{2,4},{4,4}},f=3},{136,90,55,uv={{2,2},{2,4},{4,4}},f=3},{22,60,107,uv={{2,2},{2,4},{4,4}},f=3},{30,35,23,uv={{2,2},{2,4},{4,4}},f=3},{142,63,99,uv={{2,2},{2,4},{4,4}},f=3},{143,104,101,uv={{2,2},{2,4},{4,4}},f=3},{34,30,31,uv={{2,2},{2,4},{4,4}},f=3},{45,65,123,uv={{2,2},{2,4},{4,4}},f=3},{125,121,122,uv={{2,2},{2,4},{4,4}},f=3},{56,112,71,uv={{2,2},{2,4},{4,4}},f=3},{56,71,137,uv={{2,2},{2,4},{4,4}},f=3},{25,33,26,uv={{2,2},{2,4},{4,4}},f=3},{54,81,82,uv={{2,2},{2,4},{4,4}},f=3},{58,111,83,uv={{2,2},{2,4},{4,4}},f=3},{132,138,84,uv={{2,2},{2,4},{4,4}},f=3},{132,84,116,uv={{2,2},{2,4},{4,4}},f=3},{51,134,85,uv={{2,2},{2,4},{4,4}},f=3},{61,109,86,uv={{2,2},{2,4},{4,4}},f=3},{53,88,114,uv={{2,2},{2,4},{4,4}},f=3},{141,105,89,uv={{2,2},{2,4},{4,4}},f=3},{60,96,94,uv={{2,2},{2,4},{4,4}},f=3},{62,36,140,uv={{2,2},{2,4},{4,4}},f=3},{19,108,95,uv={{2,2},{2,4},{4,4}},f=3},{19,97,61,uv={{2,2},{2,4},{4,4}},f=3},{28,26,32,uv={{2,2},{2,4},{4,4}},f=3},{99,98,106,uv={{2,2},{2,4},{4,4}},f=3},{17,135,115,uv={{2,2},{2,4},{4,4}},f=3},{22,20,107,uv={{2,2},{2,4},{4,4}},f=3},{96,59,39,uv={{2,2},{2,4},{4,4}},f=3},{96,37,95,uv={{2,2},{2,4},{4,4}},f=3},{108,17,110,uv={{2,2},{2,4},{4,4}},f=3},{36,94,58,uv={{2,2},{2,4},{4,4}},f=3},{94,111,58,uv={{2,2},{2,4},{4,4}},f=3},{93,56,137,uv={{2,2},{2,4},{4,4}},f=3},{93,92,56,uv={{2,2},{2,4},{4,4}},f=3},{92,106,112,uv={{2,2},{2,4},{4,4}},f=3},{55,54,40,uv={{2,2},{2,4},{4,4}},f=3},{55,91,54,uv={{2,2},{2,4},{4,4}},f=3},{89,105,113,uv={{2,2},{2,4},{4,4}},f=3},{114,88,41,uv={{2,2},{2,4},{4,4}},f=3},{86,78,52,uv={{2,2},{2,4},{4,4}},f=3},{86,109,78,uv={{2,2},{2,4},{4,4}},f=3},{109,24,78,uv={{2,2},{2,4},{4,4}},f=3},{110,17,115,uv={{2,2},{2,4},{4,4}},f=3},{116,131,43,uv={{2,2},{2,4},{4,4}},f=3},{84,138,83,uv={{2,2},{2,4},{4,4}},f=3},{83,111,117,uv={{2,2},{2,4},{4,4}},f=3},{54,82,130,uv={{2,2},{2,4},{4,4}},f=3},{82,50,130,uv={{2,2},{2,4},{4,4}},f=3},{82,80,50,uv={{2,2},{2,4},{4,4}},f=3},{81,57,80,uv={{2,2},{2,4},{4,4}},f=3},{79,78,77,uv={{2,2},{2,4},{4,4}},f=3},{78,55,77,uv={{2,2},{2,4},{4,4}},f=3},{133,128,42,uv={{2,2},{2,4},{4,4}},f=3},{76,53,75,uv={{2,2},{2,4},{4,4}},f=3},{131,74,127,uv={{2,2},{2,4},{4,4}},f=3},{73,51,72,uv={{2,2},{2,4},{4,4}},f=3},{72,119,48,uv={{2,2},{2,4},{4,4}},f=3},{112,132,70,uv={{2,2},{2,4},{4,4}},f=3},{120,126,44,uv={{2,2},{2,4},{4,4}},f=3},{120,69,126,uv={{2,2},{2,4},{4,4}},f=3},{69,68,46,uv={{2,2},{2,4},{4,4}},f=3},{121,120,44,uv={{2,2},{2,4},{4,4}},f=3},{121,67,120,uv={{2,2},{2,4},{4,4}},f=3},{67,47,120,uv={{2,2},{2,4},{4,4}},f=3},{67,66,47,uv={{2,2},{2,4},{4,4}},f=3},{67,49,66,uv={{2,2},{2,4},{4,4}},f=3},{123,122,44,uv={{2,2},{2,4},{4,4}},f=3},{123,65,122,uv={{2,2},{2,4},{4,4}},f=3},{65,144,125,uv={{2,2},{2,4},{4,4}},f=3},{65,64,144,uv={{2,2},{2,4},{4,4}},f=3},{144,119,125,uv={{2,2},{2,4},{4,4}},f=3},{145,45,123,uv={{2,2},{2,4},{4,4}},f=3},{145,147,45,uv={{2,2},{2,4},{4,4}},f=3},{145,146,147,uv={{2,2},{2,4},{4,4}},f=3},{148,186,188,uv={{6,5},{8,5},{6,4}},f=3},{149,184,190,uv={{6,5},{8,5},{6,4}},f=3},{150,183,192,uv={{6,5},{8,5},{6,4}},f=3},{278,189,280,uv={{6,5},{8,5},{6,4}},f=3},{151,194,216,uv={{6,5},{8,5},{6,4}},f=3},{278,280,213,uv={{6,5},{8,5},{6,4}},f=3},{177,227,226,uv={{6,5},{8,5},{6,4}},f=3},{180,229,225,uv={{6,5},{8,5},{6,4}},f=3},{179,231,224,uv={{6,5},{8,5},{6,4}},f=3},{179,232,231,uv={{6,5},{8,5},{6,4}},f=3},{176,236,222,uv={{6,5},{8,5},{6,4}},f=3},{171,242,197,uv={{6,5},{8,5},{6,4}},f=3},{169,244,243,uv={{6,5},{8,5},{6,4}},f=3},{168,246,195,uv={{6,5},{8,5},{6,4}},f=3},{168,247,246,uv={{6,5},{8,5},{6,4}},f=3},{166,249,193,uv={{6,5},{8,5},{6,4}},f=3},{163,211,257,uv={{6,5},{8,5},{6,4}},f=3},{161,209,260,uv={{6,5},{8,5},{6,4}},f=3},{159,266,185,uv={{6,5},{8,5},{6,4}},f=3},{162,210,267,uv={{6,5},{8,5},{6,4}},f=3},{160,270,208,uv={{6,5},{8,5},{6,4}},f=3},{160,271,270,uv={{6,5},{8,5},{6,4}},f=3},{158,273,207,uv={{6,5},{8,5},{6,4}},f=3},{156,205,274,uv={{6,5},{8,5},{6,4}},f=3},{155,276,275,uv={{6,5},{8,5},{6,4}},f=3},{156,182,277,uv={{6,5},{8,5},{6,4}},f=3},{182,149,206,uv={{6,5},{8,5},{6,4}},f=3},{276,156,277,uv={{6,5},{8,5},{6,4}},f=3},{204,183,150,uv={{6,5},{8,5},{6,4}},f=3},{275,157,183,uv={{6,5},{8,5},{6,4}},f=3},{273,274,159,uv={{6,5},{8,5},{6,4}},f=3},{207,273,185,uv={{6,5},{8,5},{6,4}},f=3},{272,204,209,uv={{6,5},{8,5},{6,4}},f=3},{204,150,209,uv={{6,5},{8,5},{6,4}},f=3},{270,272,161,uv={{6,5},{8,5},{6,4}},f=3},{208,187,151,uv={{6,5},{8,5},{6,4}},f=3},{208,270,187,uv={{6,5},{8,5},{6,4}},f=3},{269,211,163,uv={{6,5},{8,5},{6,4}},f=3},{267,210,212,uv={{6,5},{8,5},{6,4}},f=3},{266,267,164,uv={{6,5},{8,5},{6,4}},f=3},{265,213,165,uv={{6,5},{8,5},{6,4}},f=3},{207,278,213,uv={{6,5},{8,5},{6,4}},f=3},{263,264,265,uv={{6,5},{8,5},{6,4}},f=3},{262,206,214,uv={{6,5},{8,5},{6,4}},f=3},{206,149,214,uv={{6,5},{8,5},{6,4}},f=3},{261,262,166,uv={{6,5},{8,5},{6,4}},f=3},{261,166,193,uv={{6,5},{8,5},{6,4}},f=3},{258,260,168,uv={{6,5},{8,5},{6,4}},f=3},{258,259,260,uv={{6,5},{8,5},{6,4}},f=3},{259,161,260,uv={{6,5},{8,5},{6,4}},f=3},{257,211,216,uv={{6,5},{8,5},{6,4}},f=3},{255,257,169,uv={{6,5},{8,5},{6,4}},f=3},{256,163,257,uv={{6,5},{8,5},{6,4}},f=3},{215,255,196,uv={{6,5},{8,5},{6,4}},f=3},{254,212,218,uv={{6,5},{8,5},{6,4}},f=3},{252,254,171,uv={{6,5},{8,5},{6,4}},f=3},{217,197,153,uv={{6,5},{8,5},{6,4}},f=3},{252,171,197,uv={{6,5},{8,5},{6,4}},f=3},{246,247,248,uv={{6,5},{8,5},{6,4}},f=3},{245,221,173,uv={{6,5},{8,5},{6,4}},f=3},{243,245,173,uv={{6,5},{8,5},{6,4}},f=3},{243,244,245,uv={{6,5},{8,5},{6,4}},f=3},{197,199,153,uv={{6,5},{8,5},{6,4}},f=3},{242,174,199,uv={{6,5},{8,5},{6,4}},f=3},{239,241,175,uv={{6,5},{8,5},{6,4}},f=3},{191,279,219,uv={{6,5},{8,5},{6,4}},f=3},{191,239,279,uv={{6,5},{8,5},{6,4}},f=3},{236,238,177,uv={{6,5},{8,5},{6,4}},f=3},{237,251,238,uv={{6,5},{8,5},{6,4}},f=3},{220,152,200,uv={{6,5},{8,5},{6,4}},f=3},{234,176,222,uv={{6,5},{8,5},{6,4}},f=3},{232,173,233,uv={{6,5},{8,5},{6,4}},f=3},{224,223,154,uv={{6,5},{8,5},{6,4}},f=3},{230,203,179,uv={{6,5},{8,5},{6,4}},f=3},{229,230,179,uv={{6,5},{8,5},{6,4}},f=3},{225,224,154,uv={{6,5},{8,5},{6,4}},f=3},{229,179,224,uv={{6,5},{8,5},{6,4}},f=3},{226,227,228,uv={{6,5},{8,5},{6,4}},f=3},{201,225,154,uv={{6,5},{8,5},{6,4}},f=3},{202,235,178,uv={{6,5},{8,5},{6,4}},f=3},{198,172,235,uv={{6,5},{8,5},{6,4}},f=3},{200,237,176,uv={{6,5},{8,5},{6,4}},f=3},{213,240,165,uv={{6,5},{8,5},{6,4}},f=3},{214,250,166,uv={{6,5},{8,5},{6,4}},f=3},{214,190,250,uv={{6,5},{8,5},{6,4}},f=3},{280,253,170,uv={{6,5},{8,5},{6,4}},f=3},{280,189,253,uv={{6,5},{8,5},{6,4}},f=3},{194,259,167,uv={{6,5},{8,5},{6,4}},f=3},{187,161,259,uv={{6,5},{8,5},{6,4}},f=3},{188,268,162,uv={{6,5},{8,5},{6,4}},f=3},{188,186,268,uv={{6,5},{8,5},{6,4}},f=3},{186,271,160,uv={{6,5},{8,5},{6,4}},f=3},{181,155,271,uv={{6,5},{8,5},{6,4}},f=3},{181,276,155,uv={{6,5},{8,5},{6,4}},f=3}}
	}

	benchModel = {
		v={{-102,64,65},{-102,57,65},{-102,64,34},{-102,57,34},{64,64,65},{64,57,65},{64,64,34},{64,57,34},{-102,64,20},{-102,57,20},{-102,64,-11},{-102,57,-11},{64,64,20},{64,57,20},{64,64,-11},{64,57,-11},{-102,119,74},{-102,117,81},{-102,89,66},{-102,87,73},{64,119,74},{64,117,81},{64,89,66},{64,87,73},{43,57,76},{43,0,76},{43,57,-3},{43,0,-3},{51,57,76},{51,0,76},{51,57,-3},{51,0,-3},{51,57,65},{43,0,65},{51,0,65},{51,0,8},{43,57,65},{43,0,8},{43,43,65},{51,43,65},{51,43,8},{43,43,8},{43,114,92},{51,114,92},{51,117,81},{43,117,81},{-89,57,76},{-89,0,76},{-89,57,-3},{-89,0,-3},{-80,57,76},{-80,0,76},{-80,57,-3},{-80,0,-3},{-80,57,65},{-89,0,65},{-80,0,65},{-80,0,8},{-89,57,65},{-89,0,8},{-89,43,65},{-80,43,65},{-80,43,8},{-89,43,8},{-89,114,92},{-80,114,92},{-80,117,81},{-89,117,81}},
		f={{5,3,1,uv={{2,4},{0,2},{0,4}},f=1},{3,8,4,uv={{8,2},{6,0},{6,2}},f=1},{7,6,8,uv={{8,0},{6,2},{6,0}},f=1},{2,8,6,uv={{2,4},{0,2},{0,4}},f=1},{1,4,2,uv={{8,0},{6,2},{6,0}},f=1},{5,2,6,uv={{8,2},{6,0},{6,2}},f=1},{13,11,9,uv={{2,4},{0,2},{0,4}},f=1},{11,16,12,uv={{8,2},{6,0},{6,2}},f=1},{15,14,16,uv={{8,0},{6,2},{6,0}},f=1},{10,16,14,uv={{2,4},{0,2},{0,4}},f=1},{9,12,10,uv={{8,0},{6,2},{6,0}},f=1},{13,10,14,uv={{8,2},{6,0},{6,2}},f=1},{21,19,17,uv={{2,4},{0,2},{0,4}},f=1},{19,24,20,uv={{8,2},{6,0},{6,2}},f=1},{23,22,24,uv={{8,0},{6,2},{6,0}},f=1},{18,24,22,uv={{2,4},{0,2},{0,4}},f=1},{17,20,18,uv={{8,0},{6,2},{6,0}},f=1},{21,18,22,uv={{8,2},{6,0},{6,2}},f=1},{31,37,33,uv={{6,2},{8,4},{6,4}},f=1},{25,30,29,uv={{8,4},{6,2},{8,2}},f=1},{38,32,36,uv={{8,4},{6,2},{8,2}},f=1},{39,26,25,uv={{8,2},{6,4},{8,4}},f=1},{27,32,28,uv={{8,2},{6,4},{6,2}},f=1},{30,40,29,uv={{8,2},{6,4},{8,4}},f=1},{26,35,30,uv={{6,2},{8,4},{8,2}},f=1},{42,36,41,uv={{8,2},{6,4},{8,4}},f=1},{29,43,25,uv={{8,2},{6,4},{8,4}},f=1},{40,34,39,uv={{8,2},{6,4},{6,2}},f=1},{42,40,39,uv={{6,2},{8,4},{6,4}},f=1},{45,43,44,uv={{6,2},{8,4},{8,2}},f=1},{29,45,44,uv={{6,2},{8,4},{8,2}},f=1},{25,46,37,uv={{8,4},{6,2},{8,2}},f=1},{33,46,45,uv={{6,2},{8,4},{6,4}},f=1},{41,32,31,uv={{8,2},{6,4},{8,4}},f=1},{41,31,33,uv={{6,4},{8,4},{8,3}},f=1},{28,42,27,uv={{8,2},{6,4},{8,4}},f=1},{39,25,37,uv={{6,4},{8,4},{8,3}},f=1},{53,59,55,uv={{6,2},{8,4},{6,4}},f=1},{47,52,51,uv={{8,4},{6,2},{8,2}},f=1},{60,54,58,uv={{6,4},{8,2},{8,4}},f=1},{61,48,47,uv={{8,2},{6,4},{8,4}},f=1},{49,54,50,uv={{8,2},{6,4},{6,2}},f=1},{52,62,51,uv={{8,2},{6,4},{8,4}},f=1},{48,57,52,uv={{6,2},{8,4},{8,2}},f=1},{64,58,63,uv={{8,2},{6,4},{8,4}},f=1},{51,65,47,uv={{6,2},{8,4},{6,4}},f=1},{62,56,61,uv={{8,2},{6,4},{6,2}},f=1},{64,62,61,uv={{8,2},{6,4},{8,4}},f=1},{66,68,65,uv={{8,2},{6,4},{8,4}},f=1},{51,67,66,uv={{6,2},{8,4},{8,2}},f=1},{47,68,59,uv={{6,2},{8,4},{8,2}},f=1},{55,68,67,uv={{6,4},{8,2},{6,2}},f=1},{63,54,53,uv={{8,2},{6,4},{8,4}},f=1},{63,53,55,uv={{6,4},{8,4},{8,3}},f=1},{50,64,49,uv={{8,2},{6,4},{8,4}},f=1},{61,47,59,uv={{6,4},{8,2},{8,4}},f=1},{5,7,3,uv={{2,4},{2,2},{0,2}},f=1},{3,7,8,uv={{8,2},{8,0},{6,0}},f=1},{7,5,6,uv={{8,0},{8,2},{6,2}},f=1},{2,4,8,uv={{2,4},{2,2},{0,2}},f=1},{1,3,4,uv={{8,0},{8,2},{6,2}},f=1},{5,1,2,uv={{8,2},{8,0},{6,0}},f=1},{13,15,11,uv={{2,4},{2,2},{0,2}},f=1},{11,15,16,uv={{8,2},{8,0},{6,0}},f=1},{15,13,14,uv={{8,0},{8,2},{6,2}},f=1},{10,12,16,uv={{2,4},{2,2},{0,2}},f=1},{9,11,12,uv={{8,0},{8,2},{6,2}},f=1},{13,9,10,uv={{8,2},{8,0},{6,0}},f=1},{21,23,19,uv={{2,4},{2,2},{0,2}},f=1},{19,23,24,uv={{8,2},{8,0},{6,0}},f=1},{23,21,22,uv={{8,0},{8,2},{6,2}},f=1},{18,20,24,uv={{2,4},{2,2},{0,2}},f=1},{17,19,20,uv={{8,0},{8,2},{6,2}},f=1},{21,17,18,uv={{8,2},{8,0},{6,0}},f=1},{31,27,37,uv={{6,2},{8,2},{8,4}},f=1},{25,26,30,uv={{8,4},{6,4},{6,2}},f=1},{38,28,32,uv={{8,4},{6,4},{6,2}},f=1},{39,34,26,uv={{8,2},{6,2},{6,4}},f=1},{27,31,32,uv={{8,2},{8,4},{6,4}},f=1},{30,35,40,uv={{8,2},{6,2},{6,4}},f=1},{26,34,35,uv={{6,2},{6,4},{8,4}},f=1},{42,38,36,uv={{8,2},{6,2},{6,4}},f=1},{29,44,43,uv={{8,2},{6,2},{6,4}},f=1},{40,35,34,uv={{8,2},{8,4},{6,4}},f=1},{42,41,40,uv={{6,2},{8,2},{8,4}},f=1},{45,46,43,uv={{6,2},{6,4},{8,4}},f=1},{29,33,45,uv={{6,2},{6,4},{8,4}},f=1},{25,43,46,uv={{8,4},{6,4},{6,2}},f=1},{33,37,46,uv={{6,2},{8,2},{8,4}},f=1},{41,36,32,uv={{8,2},{6,2},{6,4}},f=1},{33,29,40,uv={{8,3},{8,2},{6,2}},f=1},{40,41,33,uv={{6,2},{6,4},{8,3}},f=1},{28,38,42,uv={{8,2},{6,2},{6,4}},f=1},{37,27,42,uv={{8,4},{8,2},{6,2}},f=1},{42,39,37,uv={{6,2},{6,4},{8,4}},f=1},{53,49,59,uv={{6,2},{8,2},{8,4}},f=1},{47,48,52,uv={{8,4},{6,4},{6,2}},f=1},{60,50,54,uv={{6,4},{6,2},{8,2}},f=1},{61,56,48,uv={{8,2},{6,2},{6,4}},f=1},{49,53,54,uv={{8,2},{8,4},{6,4}},f=1},{52,57,62,uv={{8,2},{6,2},{6,4}},f=1},{48,56,57,uv={{6,2},{6,4},{8,4}},f=1},{64,60,58,uv={{8,2},{6,2},{6,4}},f=1},{51,66,65,uv={{6,2},{8,2},{8,4}},f=1},{62,57,56,uv={{8,2},{8,4},{6,4}},f=1},{64,63,62,uv={{8,2},{6,2},{6,4}},f=1},{66,67,68,uv={{8,2},{6,2},{6,4}},f=1},{51,55,67,uv={{6,2},{6,4},{8,4}},f=1},{47,65,68,uv={{6,2},{6,4},{8,4}},f=1},{55,59,68,uv={{6,4},{8,4},{8,2}},f=1},{63,58,54,uv={{8,2},{6,2},{6,4}},f=1},{55,51,62,uv={{8,3},{8,2},{6,2}},f=1},{62,63,55,uv={{6,2},{6,4},{8,3}},f=1},{50,60,64,uv={{8,2},{6,2},{6,4}},f=1},{59,49,64,uv={{8,4},{8,2},{6,2}},f=1},{64,61,59,uv={{6,2},{6,4},{8,4}},f=1}}
	}

	interactModel = {
		v={
			{ 106,-17,-64},
			{-106,-17,-64},
			{ 106, 17,-64},
			{-106, 17,-64},
		},
		f={
			{1,4,2,uv={{86,72},{32,64},{32,72},2},f=2},
			{1,3,4,uv={{86,72},{86,64},{32,64},2},f=2},
		}
	}
end

local tree = obj.new(treeModel)
local bench = obj.new(benchModel)
local interact = obj.new(interactModel)

-- object data
interact.position = vec3(64,128,92)
interact.yaw = 0

local benchText = {
	{
		"Sitting on a park bench, you can",
		"hear the whisper of the wind, which",
		"tells the secrets of old trees and",
		"encourages you to forget about time.",
		"But don't forget to get up on time,",
		"the bench also has its time!"
	},
	{
		"Sitting on a bench, think about how we",
		"all strive to find our places in this",
		"endless symphony of life, and how sometimes",
		"this place can be next to a bird that has",
		"decided to sit on your bench."
	},
	{
		"Sitting on a bench, you become a time",
		"magician - a lonely observer, slowing",
		"down the flow of the world with your",
		"presence. But remember that time also",
		"likes to make jokes, especially when",
		"you're busy watching birds."
	},
	{
		"Not only you are sitting on the bench,",
		"but also the shadows of the past. In the",
		"pixel park, each pixel is a separate story,",
		"and each shadow is a mute testimony to",
		"pixel adventures. Save your pixel peace,",
		"beause even in this retro world pixels",
		"can have theis secrets."
	},
	{
		"In this pixel world, you become a code",
		"architect by compiling your thoughts",
		"as lines in a program. Think of life",
		"as a big program: every decision is",
		"a function, every choise is a condiction.",
		"But remember, unlike code, life does",
		"not forgive mistakes."
	}
}

-- tree matrices
local treeMat = {}

for i = 1, 5 do
	local mat = matrix.translation(math.random(-1000,1000),0,math.random(-1000,1000))

	mat = mat * matrix.rotationY(math.random()*math.pi*2)

	treeMat[i] = mat
end

local plr = {
	speed = 0.1,
	pos = vec3(0,128,-512),
	prevPos = vec3(),

	vel = vec3(),

	fly = false,
	sitted = false,
	sitted_t = 30,

	text = {
		selected = math.random(1, #benchText),
		line = 1,
		col = 1,
		t = 0,
	},

	height = 128,

	faces = true,
	frame = false,
	points = false,
	debug = false,
}

local function playerUpdate()
	local mx,my = mouse()

	-- Player movement
	local plrVec = vec3()

	if key(23) then plrVec.z = plrVec.z + 1 end
	if key(19) then plrVec.z = plrVec.z - 1 end
	if key(1)  then plrVec.x = plrVec.x - 1 end
	if key(4)  then plrVec.x = plrVec.x + 1 end

	-- Speed
	if key(64) then plr.speed = 20 else plr.speed = 10 end

	if keyp(22) then plr.fly = not plr.fly end

	if plr.sitted then
		plr.vel = vec3()
		plr.pos = lerp(plr.pos, vec3(0,plr.height, 32), 0.1)

		if plr.sitted_t < 30 then
			Ara.camera.yaw = lerp(Ara.camera.yaw, math.pi, 0.1)
		end
	else
		if plr.sitted_t < 30 then
			plr.pos = lerp(plr.pos, plr.prevPos, 0.1)
		end

		if plr.fly then
			if key(63) then plrVec.y = plrVec.y - 1 end
			if key(48) then plrVec.y = plrVec.y + 1 end
			
			plrVec = plrVec:normalize()
			plrVec = plrVec * matrix.rotationY(-Ara.camera.yaw)
			plrVec = plrVec * plr.speed * 2

			plr.vel = lerp(plr.vel, plrVec, 0.1)
		else
			-- jumping
			if plr.pos.y <= plr.height and keyp(48) then plr.vel.y = 15 end

			-- gravity
			if plr.pos.y > plr.height then
				plr.vel.y = plr.vel.y - 1
			end

			if plr.pos.y + plr.vel.y < plr.height then
				plr.pos.y = plr.height
				plr.vel.y = 0
			end
			
			plrVec = plrVec:normalize()
			plrVec = plrVec * matrix.rotationY(-Ara.camera.yaw)
			plrVec = plrVec * plr.speed

			plr.vel.x = lerp(plr.vel.x, plrVec.x, 0.2)
			plr.vel.z = lerp(plr.vel.z, plrVec.z, 0.2)
		end
	end


	plr.pos = plr.pos + plr.vel

	Ara.camera.pos = plr.pos

	-- Debug
	if keyp(49) then plr.debug = not plr.debug end
	if keyp(28) then plr.faces = not plr.faces end
	if keyp(29) then plr.frame = not plr.frame end
	if keyp(30) then plr.points = not plr.points end

	-- Camera rotation
	Ara.camera.pitch = Ara.camera.pitch - my / 80
	Ara.camera.yaw   = Ara.camera.yaw   - mx / 80

	-- Camera rotation restriction
	Ara.camera.yaw = Ara.camera.yaw % (math.pi*2)
	Ara.camera.pitch = clamp(Ara.camera.pitch, -math.pi/2, math.pi/2)
end

poke(0x7FC3F,1,1)
function TIC()
	local start = time()

	-- Updating
	playerUpdate()

	-- Rendering
	Ara.drawSky(13, 5)

	Ara.matrix = matrix.camera()

	for i = 1, #treeMat do
		tree.matrix = treeMat[i]
		tree:draw(plr.faces, plr.frame, plr.points)
	end

	bench:drawPos(0,0,0,plr.faces,plr.frame,plr.points)

	
	if plr.sitted then
		-- text rendering
		plr.text.t = plr.text.t + 1

		local text = benchText[plr.text.selected]
		for i = 1, math.min(plr.text.line, #text) do
			local str = text[i]

			if i == plr.text.line then
				str = str:sub(1,plr.text.col)

				if plr.text.t == 3 then
					plr.text.t = 0
					plr.text.col = plr.text.col + 1
					if plr.text.col == #text[i] then
						plr.text.line = plr.text.line + 1
						plr.text.col = 0
					end
				end
			end

			print(str, 2, 65 + i*8, 1, false, 1, true)
			print(str, 2, 64 + i*8, 15, false, 1, true)
		end

		--
		if keyp(5) then
			plr.sitted = false
			plr.sitted_t = 0
			plr.text.selected = math.random(1, #benchText)
		end
	else
		-- interact sign
		if plr.pos:length() < 256 then
			local dir = (interact.position - plr.pos):normalize()

			local angle = math.atan(dir.z, dir.x)

			interact.yaw = lerp(interact.yaw, angle, 0.1)

			interact.position.x = clamp(lerp(interact.position.x, plr.pos.x, 0.05), -92, 92)

			interact:initMat()
			interact:rotateY(-interact.yaw + math.pi/2)
			interact:pos(interact.position)
			interact:draw(plr.faces,plr.frame,plr.points)

			if keyp(5) then
				plr.sitted = true
				plr.sitted_t = 0
				plr.prevPos = plr.pos

				plr.text.t = 0
				plr.text.line = 1
				plr.text.col = 1
			end
		end
	end

	plr.sitted_t = plr.sitted_t + 1

	-- Debug
	local frame = time() - start

	if plr.debug then
		local debug_text = {
			"Press [tab] to close",
			string.format("Ara-ara engine v. %s",tostring(Ara.version)),
			string.format("X: %0.1f Y: %0.1f Z: %0.1f", Ara.camera.pos.x, Ara.camera.pos.y, Ara.camera.pos.z),
			string.format("X: %0.1f Y: %0.1f Z: %0.1f", plr.vel.x, plr.vel.y, plr.vel.z),
			string.format("Camera: %0.1f %0.1f",math.deg(Ara.camera.yaw), math.deg(Ara.camera.pitch)),
			string.format("frame %.1f ms.",frame),
		}

		for i = 1,#debug_text do
			local text = debug_text[i]
			local text_len = print(text, 240, 0)
			rect(0,(i-1)*8,text_len+1,8,1)
			print(text,1,(i-1)*8+2,0)
			print(text,1,(i-1)*8+1,15)
		end
	end
end

-- <TILES>
-- 000:001122330011223344556677445566778899aabb8899aabbccddeeffccddeeff
-- 001:7777777771111111711111117111111171111111711111117111111171111111
-- 002:7777777711111111111111111111111111111111111111111111111111111111
-- 003:7777777711111117111111171111111711111117111111171111111711111117
-- 004:222222222888888828fff8882888f88f288f88ff28f8888f28fff88828888888
-- 005:22222222888888888888888888888888f8888888888888888888888888888888
-- 006:2222222288888888888888888888888888888888888888888888888888888888
-- 007:2222222288888882888888828888888288888882888888828888888288888882
-- 008:555555555bbbbbbb5bfbfbbb5bfbfbbf5bbffbff5bbbfbbf5bffbbbb5bbbbbbb
-- 009:55555555bbbbbbbbbbbbbbbbbbbbbbbbfbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 010:55555555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 011:55555555bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5
-- 012:666666666ccccccc6cfcfccc6cfcfccf6ccfccff6cfcfccf6cfcfccc6ccccccc
-- 013:66666666ccccccccccccccccccccccccfccccccccccccccccccccccccccccccc
-- 014:66666666cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 015:66666666ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6
-- 017:7111111171111111711111117111111171111111711111117777777700000000
-- 018:1111111111111111111111111111111111111111111111117777777700000000
-- 019:1111111711111117111111171111111711111117111111177777777700000000
-- 020:2888888828888888288888882888888828888888288888882888888828888888
-- 021:8888888888888888888888888888888888888888888888888888888888888888
-- 022:8888888888888888888888888888888888888888888888888888888888888888
-- 023:8888888288888882888888828888888288888882888888828888888288888882
-- 024:5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb
-- 025:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 026:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 027:bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5
-- 028:6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc
-- 029:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 030:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 031:ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6
-- 036:2888888828888888288888882888888828888888288888882888888828888888
-- 037:8888888888888888888888888888888888888888888888888888888888888888
-- 038:8888888888888888888888888888888888888888888888888888888888888888
-- 039:8888888288888882888888828888888288888882888888828888888288888882
-- 040:5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb
-- 041:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 042:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 043:bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5
-- 044:6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc
-- 045:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 046:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 047:ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6
-- 052:2888888828888888288888882888888828888888288888882888888822222222
-- 053:8888888888888888888888888888888888888888888888888888888822222222
-- 054:8888888888888888888888888888888888888888888888888888888822222222
-- 055:8888888288888882888888828888888288888882888888828888888222222222
-- 056:5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb55555555
-- 057:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb55555555
-- 058:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb55555555
-- 059:bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb5bbbbbbb555555555
-- 060:6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc66666666
-- 061:cccccccccccccccccccccccccccccccccccccccccccccccccccccccc66666666
-- 062:cccccccccccccccccccccccccccccccccccccccccccccccccccccccc66666666
-- 063:ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc6ccccccc666666666
-- 068:888888888222222282fff2228222f222822f22ff82f2222282fff22282222222
-- 069:88888888222222222222222222222222f2222222222222222222222222222222
-- 070:8888888822222222222222222222222222222222222222222222222222222222
-- 071:8888888822222228222222282222222822222228222222282222222822222228
-- 072:bbbbbbbbb5555555b5f5f555b5f5f555b55ff5ffb555f555b5ff5555b5555555
-- 073:bbbbbbbb555555555555555555555555f5555555555555555555555555555555
-- 074:bbbbbbbb55555555555555555555555555555555555555555555555555555555
-- 075:bbbbbbbb5555555b5555555b5555555b5555555b5555555b5555555b5555555b
-- 076:ccccccccc6666666c6f6f666c6f6f666c66f66ffc6f6f666c6f6f666c6666666
-- 077:cccccccc666666666666666666666666f6666666666666666666666666666666
-- 078:cccccccc66666666666666666666666666666666666666666666666666666666
-- 079:cccccccc6666666c6666666c6666666c6666666c6666666c6666666c6666666c
-- 084:8222222282222222822222228222222282222222822222228222222282222222
-- 085:2222222222222222222222222222222222222222222222222222222222222222
-- 086:2222222222222222222222222222222222222222222222222222222222222222
-- 087:2222222822222228222222282222222822222228222222282222222822222228
-- 088:b5555555b5555555b5555555b5555555b5555555b5555555b5555555b5555555
-- 089:5555555555555555555555555555555555555555555555555555555555555555
-- 090:5555555555555555555555555555555555555555555555555555555555555555
-- 091:5555555b5555555b5555555b5555555b5555555b5555555b5555555b5555555b
-- 092:c6666666c6666666c6666666c6666666c6666666c6666666c6666666c6666666
-- 093:6666666666666666666666666666666666666666666666666666666666666666
-- 094:6666666666666666666666666666666666666666666666666666666666666666
-- 095:6666666c6666666c6666666c6666666c6666666c6666666c6666666c6666666c
-- 100:8222222282222222822222228222222282222222822222228222222282222222
-- 101:2222222222222222222222222222222222222222222222222222222222222222
-- 102:2222222222222222222222222222222222222222222222222222222222222222
-- 103:2222222822222228222222282222222822222228222222282222222822222228
-- 104:b5555555b5555555b5555555b5555555b5555555b5555555b5555555b5555555
-- 105:5555555555555555555555555555555555555555555555555555555555555555
-- 106:5555555555555555555555555555555555555555555555555555555555555555
-- 107:5555555b5555555b5555555b5555555b5555555b5555555b5555555b5555555b
-- 108:c6666666c6666666c6666666c6666666c6666666c6666666c6666666c6666666
-- 109:6666666666666666666666666666666666666666666666666666666666666666
-- 110:6666666666666666666666666666666666666666666666666666666666666666
-- 111:6666666c6666666c6666666c6666666c6666666c6666666c6666666c6666666c
-- 116:8222222282222222822222228222222282222222822222228222222288888888
-- 117:2222222222222222222222222222222222222222222222222222222288888888
-- 118:2222222222222222222222222222222222222222222222222222222288888888
-- 119:2222222822222228222222282222222822222228222222282222222888888888
-- 120:b5555555b5555555b5555555b5555555b5555555b5555555b5555555bbbbbbbb
-- 121:55555555555555555555555555555555555555555555555555555555bbbbbbbb
-- 122:55555555555555555555555555555555555555555555555555555555bbbbbbbb
-- 123:5555555b5555555b5555555b5555555b5555555b5555555b5555555bbbbbbbbb
-- 124:c6666666c6666666c6666666c6666666c6666666c6666666c6666666cccccccc
-- 125:66666666666666666666666666666666666666666666666666666666cccccccc
-- 126:66666666666666666666666666666666666666666666666666666666cccccccc
-- 127:6666666c6666666c6666666c6666666c6666666c6666666c6666666ccccccccc
-- 132:111111121fffff171ff000171ffff1171ff001171fffff171000001711111111
-- 133:222222227ff7777777777fff7ff77ff77ff77ff77ff77ff77777777711111111
-- 134:222222227777ff77f77fffff7f77ff777f77ff777f777fff7777777711111111
-- 135:222222227777777777fff77f7ff7ff7f7fff777f77fff77f7777777711111111
-- 136:2222222277777777fff777fff77f7f77f7777f77f77777ff7777777711111111
-- 137:2222222277777777ff77fff7ff7ff777ff7ff777ff77fff77777777711111111
-- 138:222222227ff77722fffff7227ff777227ff7772277fff7227777772211111122
-- </TILES>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
-- </SFX>

-- <TRACKS>
-- 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </TRACKS>

-- <PALETTE>
-- 000:000000383c4c345784493c2ba4642261893c8126336d69717da2f2eb89319d9db29dce50ea3040b2dceff7e26bffffff
-- </PALETTE>

