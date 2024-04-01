-- title:  Ara-ara 3D engine
-- author: HanamileH
-- desc:   version 1.0 beta
-- script: lua

-- github: todo

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
		-- if matrix
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
	-- I really apologize for this kind of code,
	-- but it's noticeably faster without loops

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

	projection = function()
		local tg = math.tan(math.rad(Ara.camera.fov/2))

		return matrix.new({
			{1/tg,0   ,0,0},
			{0   ,1/tg,0,0},
			{0   ,0   ,1,0},
			{0   ,0   ,1,0}
		})
	end,

	-- Creating a ready-made camera matrix
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
	version = "1.0 beta",

	camera = {
		fov = 120,

		near = 0.1,

		pitch = 0,
		yaw = math.pi/2,
		roll = 0,

		pos = vec3(),
	},
	
	wireframeColorDefault = 0,
	pointsColorDefault = 3,
	pointsRadiusDefault = 0,

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

-- note that this function does not use the world matrix by default
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
			x=x/w*120 + 120, y=-y/w*120 + 68, z=z/w, w=w,
			x2=x, y2=-y, z2=z
		}
	end

	return result
end

function Ara.drawPoints(vertices, color, radius)
	color = color or 0

	for i = 1, #vertices do
		local v = vertices[i]

		if v.z2 > Ara.camera.near then
			pix(v.x, v.y, color)
			if radius then circb(v.x, v.y, radius, color) end
		end
	end
end

function Ara.drawFaces(vertices, faces)
	local near = Ara.camera.near

	for i = 1, #faces do
		local poly = faces[i]

		local v1 = vertices[poly[1]]
		local v2 = vertices[poly[2]]
		local v3 = vertices[poly[3]]

		local uv = poly.uv

		local triPoints=(v2.x-v1.x) * (v3.y-v1.y) - (v3.x-v1.x) * (v2.y-v1.y) > 0
		local drawTri = (poly.f==3) or (triPoints == (poly.f==1))

		local z = {v1.z2>near,v2.z2>near,v3.z2>near}

		if not poly.f == 3 then
			drawTri = (drawTri ~= (v1.z2>0) ~= (v2.z2>0) ~= (v3.z2>0))
		end

		if drawTri and (z[1] or z[2] or z[3]) then
			if z[1] and z[2] and z[3] then
				ttri(v1.x, v1.y,
					v2.x, v2.y,
					v3.x, v3.y,
					uv[1][1], uv[1][2],
					uv[2][1], uv[2][2],
					uv[3][1], uv[3][2], 0, uv[4],
					v1.z2, v2.z2, v3.z2
				)
			else
				-- Z-clipping
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
		end
	end
end

function Ara.drawWireframe(vertices, faces, color)
	local near = Ara.camera.near

	for i = 1, #faces do
		local poly = faces[i]

		local v1 = vertices[poly[1]]
		local v2 = vertices[poly[2]]
		local v3 = vertices[poly[3]]

		local z = {v1.z2>near,v2.z2>near,v3.z2>near}

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
			Ara.drawFaces(processed, self.model.f)
		end

		-- Rendering wireframe
		if wireframe then
			wireframeColor = wireframeColor or Ara.wireframeColorDefault
			Ara.drawWireframe(processed, self.model.f, wireframeColor)
		end

		-- Rendering vertices
		if points then
			pointsColor = pointsColor or Ara.pointsColorDefault
			pointsRadius = pointsRadius or Ara.pointsRadiusDefault
			Ara.drawPoints(processed, pointsColor, pointsRadius)
		end
	end

	function object:drawPos(...)
		local args = {...}
		local pos
		if type(args[1]) ~= "table" then
			pos = vec3(args[1], args[2], args[3])

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


local cube_model = {
	v={
		{ 1, 1, 1},
		{ 1,-1, 1},
		{ 1, 1,-1},
		{ 1,-1,-1},
		{-1, 1, 1},
		{-1,-1, 1},
		{-1, 1,-1},
		{-1,-1,-1},
	},
	f={
		{5,3,1,uv={{96 ,0 },{64,32},{96 ,32},-1},f=3},
		{3,8,4,uv={{64 ,32},{32,64},{64 ,64},-1},f=3},
		{7,6,8,uv={{128,32},{96,64},{128,64},-1},f=3},
		{2,8,6,uv={{96 ,32},{64,64},{96 ,64},-1},f=3},
		{1,4,2,uv={{128,0 },{96,32},{128,32},-1},f=3},
		{5,2,6,uv={{64 ,0 },{32,32},{64 ,32},-1},f=3},
		{5,7,3,uv={{96 ,0 },{64,0 },{64 ,32},-1},f=3},
		{3,7,8,uv={{64 ,32},{32,32},{32 ,64},-1},f=3},
		{7,5,6,uv={{128,32},{96,32},{96 ,64},-1},f=3},
		{2,4,8,uv={{96 ,32},{64,32},{64 ,64},-1},f=3},
		{1,3,4,uv={{128,0 },{96,0 },{96 ,32},-1},f=3},
		{5,1,2,uv={{64 ,0 },{32,0 },{32 ,32},-1},f=3},
	}
}

local cube = obj.new(cube_model)

local plr = {
	speed = 0.1,
	faces = true,
	frame = false,
	points = false,
	debug = true
}

local function playerUpdate()
	local mx,my = mouse()

	-- Camera movement
	local camVec = vec3()

	if key(23) then camVec.z = camVec.z + 1 end
	if key(19) then camVec.z = camVec.z - 1 end
	if key(1)  then camVec.x = camVec.x - 1 end
	if key(4)  then camVec.x = camVec.x + 1 end
	if key(48) then camVec.y = camVec.y + 1 end
	if key(63) then camVec.y = camVec.y - 1 end

	-- Speed
	if key(64) then plr.speed = 0.5 else plr.speed = 0.1 end
	if key(65) then plr.speed = 0.01 end


	camVec = camVec:normalize()
	camVec = camVec * matrix.rotationY(-Ara.camera.yaw)
	camVec = camVec * plr.speed

	Ara.camera.pos = Ara.camera.pos + camVec

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
	Ara.drawSky(11, 14)

	Ara.matrix = matrix.camera()

	cube:draw()
	-- Debug
	local frame = time() - start

	if plr.debug then
		local debug_text = {
			"Press [tab] to close",
			string.format("Ara-ara engine v. %s",tostring(Ara.version)),
			string.format("X: %0.1f Y: %0.1f Z: %0.1f", Ara.camera.pos.x, Ara.camera.pos.y, Ara.camera.pos.z),
			string.format("Camera: %0.1f %0.1f",math.deg(Ara.camera.yaw), math.deg(Ara.camera.pitch)),
			string.format("frame %.1f ms.",frame),
		}

		for i = 1,#debug_text do
			local text = debug_text[i]
			local text_len = print(text, 240, 0)
			rect(0,(i-1)*8,text_len+1,8,15)
			print(text,1,(i-1)*8+2,0)
			print(text,1,(i-1)*8+1,12)
		end
	end
end

-- <TILES>
-- 000:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 001:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 002:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 003:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 004:888888888999999989ccc9998999c99c899c99cc89c9999c89ccc99989999999
-- 005:88888888999999999999999999999999c9999999999999999999999999999999
-- 006:8888888899999999999999999999999999999999999999999999999999999999
-- 007:8888888899999998999999989999999899999998999999989999999899999998
-- 008:777777777666666676c6c66676c6c66c766cc6cc7666c66c76cc666676666666
-- 009:77777777666666666666666666666666c6666666666666666666666666666666
-- 010:7777777766666666666666666666666666666666666666666666666666666666
-- 011:7777777766666667666666676666666766666667666666676666666766666667
-- 012:111111111222222212c2c22212c2c22c122c22cc12c2c22c12c2c22212222222
-- 013:11111111222222222222222222222222c2222222222222222222222222222222
-- 014:1111111122222222222222222222222222222222222222222222222222222222
-- 015:1111111122222221222222212222222122222221222222212222222122222221
-- 016:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 017:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 018:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 019:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 020:8999999989999999899999998999999989999999899999998999999989999999
-- 021:9999999999999999999999999999999999999999999999999999999999999999
-- 022:9999999999999999999999999999999999999999999999999999999999999999
-- 023:9999999899999998999999989999999899999998999999989999999899999998
-- 024:7666666676666666766666667666666676666666766666667666666676666666
-- 025:6666666666666666666666666666666666666666666666666666666666666666
-- 026:6666666666666666666666666666666666666666666666666666666666666666
-- 027:6666666766666667666666676666666766666667666666676666666766666667
-- 028:1222222212222222122222221222222212222222122222221222222212222222
-- 029:2222222222222222222222222222222222222222222222222222222222222222
-- 030:2222222222222222222222222222222222222222222222222222222222222222
-- 031:2222222122222221222222212222222122222221222222212222222122222221
-- 032:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 033:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 034:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 035:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 036:8999999989999999899999998999999989999999899999998999999989999999
-- 037:9999999999999999999999999999999999999999999999999999999999999999
-- 038:9999999999999999999999999999999999999999999999999999999999999999
-- 039:9999999899999998999999989999999899999998999999989999999899999998
-- 040:7666666676666666766666667666666676666666766666667666666676666666
-- 041:6666666666666666666666666666666666666666666666666666666666666666
-- 042:6666666666666666666666666666666666666666666666666666666666666666
-- 043:6666666766666667666666676666666766666667666666676666666766666667
-- 044:1222222212222222122222221222222212222222122222221222222212222222
-- 045:2222222222222222222222222222222222222222222222222222222222222222
-- 046:2222222222222222222222222222222222222222222222222222222222222222
-- 047:2222222122222221222222212222222122222221222222212222222122222221
-- 048:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 049:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 050:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 051:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 052:8999999989bbb9bb899b99b9899b99bb899b99b9899b99bb8999999988888888
-- 053:99999999b9b9b9bb99b9b99bb99b999b99b9b99bb9b9b99b9999999988888888
-- 054:99999999b9999999999999999999999999999999999999999999999988888888
-- 055:9999999899999998999999989999999899999998999999989999999888888888
-- 056:7666666676555655766566567665665576656656766566557666666677777777
-- 057:6666666656565655665656655665666566565665565656656666666677777777
-- 058:6666666656666666666666666666666666666666666666666666666677777777
-- 059:6666666766666667666666676666666766666667666666676666666777777777
-- 060:1222222212ccc2cc122c22c2122c22cc122c22c2122c22cc1222222211111111
-- 061:22222222c2c2c2cc22c2c22cc22c222c22c2c22cc2c2c22c2222222211111111
-- 062:22222222c2222222222222222222222222222222222222222222222211111111
-- 063:2222222122222221222222212222222122222221222222212222222111111111
-- 064:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 065:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 066:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 067:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 068:999999999888888898ccc8889888c888988c88cc98c8888898ccc88898888888
-- 069:99999999888888888888888888888888c8888888888888888888888888888888
-- 070:9999999988888888888888888888888888888888888888888888888888888888
-- 071:9999999988888889888888898888888988888889888888898888888988888889
-- 072:666666666777777767c7c77767c7c777677cc7cc6777c77767cc777767777777
-- 073:66666666777777777777777777777777c7777777777777777777777777777777
-- 074:6666666677777777777777777777777777777777777777777777777777777777
-- 075:6666666677777776777777767777777677777776777777767777777677777776
-- 076:222222222111111121c1c11121c1c111211c11cc21c1c11121c1c11121111111
-- 077:22222222111111111111111111111111c1111111111111111111111111111111
-- 078:2222222211111111111111111111111111111111111111111111111111111111
-- 079:2222222211111112111111121111111211111112111111121111111211111112
-- 080:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 081:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 082:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 083:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 084:9888888898888888988888889888888898888888988888889888888898888888
-- 085:8888888888888888888888888888888888888888888888888888888888888888
-- 086:8888888888888888888888888888888888888888888888888888888888888888
-- 087:8888888988888889888888898888888988888889888888898888888988888889
-- 088:6777777767777777677777776777777767777777677777776777777767777777
-- 089:7777777777777777777777777777777777777777777777777777777777777777
-- 090:7777777777777777777777777777777777777777777777777777777777777777
-- 091:7777777677777776777777767777777677777776777777767777777677777776
-- 092:2111111121111111211111112111111121111111211111112111111121111111
-- 093:1111111111111111111111111111111111111111111111111111111111111111
-- 094:1111111111111111111111111111111111111111111111111111111111111111
-- 095:1111111211111112111111121111111211111112111111121111111211111112
-- 096:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 097:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 098:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 099:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 100:9888888898888888988888889888888898888888988888889888888898888888
-- 101:8888888888888888888888888888888888888888888888888888888888888888
-- 102:8888888888888888888888888888888888888888888888888888888888888888
-- 103:8888888988888889888888898888888988888889888888898888888988888889
-- 104:6777777767777777677777776777777767777777677777776777777767777777
-- 105:7777777777777777777777777777777777777777777777777777777777777777
-- 106:7777777777777777777777777777777777777777777777777777777777777777
-- 107:7777777677777776777777767777777677777776777777767777777677777776
-- 108:2111111121111111211111112111111121111111211111112111111121111111
-- 109:1111111111111111111111111111111111111111111111111111111111111111
-- 110:1111111111111111111111111111111111111111111111111111111111111111
-- 111:1111111211111112111111121111111211111112111111121111111211111112
-- 112:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 113:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 114:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 115:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 116:9888888898999899988988989889889998898898988988999888888899999999
-- 117:8888888898989899889898899889888988989889989898898888888899999999
-- 118:8888888898888888888888888888888888888888888888888888888899999999
-- 119:8888888988888889888888898888888988888889888888898888888999999999
-- 120:6777777767666766677677676776776667767767677677666777777766666666
-- 121:7777777767676766776767766776777677676776676767767777777766666666
-- 122:7777777767777777777777777777777777777777777777777777777766666666
-- 123:7777777677777776777777767777777677777776777777767777777666666666
-- 124:2111111121ccc1cc211c11c1211c11cc211c11c1211c11cc2111111122222222
-- 125:11111111c1c1c1cc11c1c11cc11c111c11c1c11cc1c1c11c1111111122222222
-- 126:11111111c1111111111111111111111111111111111111111111111122222222
-- 127:1111111211111112111111121111111211111112111111121111111222222222
-- 128:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 129:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 130:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 131:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 132:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 133:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 134:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 135:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 136:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 137:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 138:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 139:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 140:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 141:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 142:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 143:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 144:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 145:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 146:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 147:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 148:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 149:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 150:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 151:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 152:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 153:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 154:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 155:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 156:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 157:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 158:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 159:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 160:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 161:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 162:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 163:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 164:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 165:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 166:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 167:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 168:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 169:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 170:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 171:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 172:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 173:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 174:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 175:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 176:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 177:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 178:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 179:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 180:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 181:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 182:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 183:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 184:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 185:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 186:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 187:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 188:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 189:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 190:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 191:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 192:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 193:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 194:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 195:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 196:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 197:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 198:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 199:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 200:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 201:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 202:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 203:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 204:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 205:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 206:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 207:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 208:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 209:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 210:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 211:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 212:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 213:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 214:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 215:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 216:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 217:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 218:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 219:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 220:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 221:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 222:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 223:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 224:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 225:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 226:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 227:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 228:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 229:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 230:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 231:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 232:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 233:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 234:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 235:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 236:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 237:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 238:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 239:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 240:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 241:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 242:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 243:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 244:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 245:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 246:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 247:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 248:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 249:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 250:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 251:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 252:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 253:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 254:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 255:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
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
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

