//----------------------------------------------------------------------------------
// Module Functions Definition - Utils math
//----------------------------------------------------------------------------------

// Clamp float value
RMAPI float Clamp(float value, float min, float max)

// Calculate linear interpolation between two floats
RMAPI float Lerp(float start, float end, float amount)

// Normalize input value within input range
RMAPI float Normalize(float value, float start, float end)

// Remap input value within input range to output range
RMAPI float Remap(float value, float inputStart, float inputEnd, float outputStart, float outputEnd)

// Wrap input value from min to max
RMAPI float Wrap(float value, float min, float max)

// Check whether two given floats are almost equal
RMAPI int FloatEquals(float x, float y)

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector2 math
//----------------------------------------------------------------------------------

// Vector with components value 0.0f
RMAPI Vector2 Vector2Zero(void)

// Vector with components value 1.0f
RMAPI Vector2 Vector2One(void)

// Add two vectors (v1 + v2)
RMAPI Vector2 Vector2Add(Vector2 v1, Vector2 v2)

// Add vector and float value
RMAPI Vector2 Vector2AddValue(Vector2 v, float add)

// Subtract two vectors (v1 - v2)
RMAPI Vector2 Vector2Subtract(Vector2 v1, Vector2 v2)

// Subtract vector by float value
RMAPI Vector2 Vector2SubtractValue(Vector2 v, float sub)

// Calculate vector length
RMAPI float Vector2Length(Vector2 v)

// Calculate vector square length
RMAPI float Vector2LengthSqr(Vector2 v)

// Calculate two vectors dot product
RMAPI float Vector2DotProduct(Vector2 v1, Vector2 v2)

// Calculate distance between two vectors
RMAPI float Vector2Distance(Vector2 v1, Vector2 v2)

// Calculate square distance between two vectors
RMAPI float Vector2DistanceSqr(Vector2 v1, Vector2 v2)

// Calculate angle between two vectors
// NOTE: Angle is calculated from origin point (0, 0)
RMAPI float Vector2Angle(Vector2 v1, Vector2 v2)

// Calculate angle defined by a two vectors line
// NOTE: Parameters need to be normalized
// Current implementation should be aligned with glm::angle
RMAPI float Vector2LineAngle(Vector2 start, Vector2 end)

// Scale vector (multiply by value)
RMAPI Vector2 Vector2Scale(Vector2 v, float scale)

// Multiply vector by vector
RMAPI Vector2 Vector2Multiply(Vector2 v1, Vector2 v2)

// Negate vector
RMAPI Vector2 Vector2Negate(Vector2 v)

// Divide vector by vector
RMAPI Vector2 Vector2Divide(Vector2 v1, Vector2 v2)

// Normalize provided vector
RMAPI Vector2 Vector2Normalize(Vector2 v)

// Transforms a Vector2 by a given Matrix
RMAPI Vector2 Vector2Transform(Vector2 v, Matrix mat)

// Calculate linear interpolation between two vectors
RMAPI Vector2 Vector2Lerp(Vector2 v1, Vector2 v2, float amount)

// Calculate reflected vector to normal
RMAPI Vector2 Vector2Reflect(Vector2 v, Vector2 normal)

// Get min value for each pair of components
RMAPI Vector2 Vector2Min(Vector2 v1, Vector2 v2)

// Get max value for each pair of components
RMAPI Vector2 Vector2Max(Vector2 v1, Vector2 v2)

// Rotate vector by angle
RMAPI Vector2 Vector2Rotate(Vector2 v, float angle)

// Move Vector towards target
RMAPI Vector2 Vector2MoveTowards(Vector2 v, Vector2 target, float maxDistance)

// Invert the given vector
RMAPI Vector2 Vector2Invert(Vector2 v)

// Clamp the components of the vector between
// min and max values specified by the given vectors
RMAPI Vector2 Vector2Clamp(Vector2 v, Vector2 min, Vector2 max)

// Clamp the magnitude of the vector between two min and max values
RMAPI Vector2 Vector2ClampValue(Vector2 v, float min, float max)

// Check whether two given vectors are almost equal
RMAPI int Vector2Equals(Vector2 p, Vector2 q)

// Compute the direction of a refracted ray
// v: normalized direction of the incoming ray
// n: normalized normal vector of the interface of two optical media
// r: ratio of the refractive index of the medium from where the ray comes
//    to the refractive index of the medium on the other side of the surface
RMAPI Vector2 Vector2Refract(Vector2 v, Vector2 n, float r)


//----------------------------------------------------------------------------------
// Module Functions Definition - Vector3 math
//----------------------------------------------------------------------------------

// Vector with components value 0.0f
RMAPI Vector3 Vector3Zero(void)

// Vector with components value 1.0f
RMAPI Vector3 Vector3One(void)

// Add two vectors
RMAPI Vector3 Vector3Add(Vector3 v1, Vector3 v2)

// Add vector and float value
RMAPI Vector3 Vector3AddValue(Vector3 v, float add)

// Subtract two vectors
RMAPI Vector3 Vector3Subtract(Vector3 v1, Vector3 v2)

// Subtract vector by float value
RMAPI Vector3 Vector3SubtractValue(Vector3 v, float sub)

// Multiply vector by scalar
RMAPI Vector3 Vector3Scale(Vector3 v, float scalar)

// Multiply vector by vector
RMAPI Vector3 Vector3Multiply(Vector3 v1, Vector3 v2)

// Calculate two vectors cross product
RMAPI Vector3 Vector3CrossProduct(Vector3 v1, Vector3 v2)

// Calculate one vector perpendicular vector
RMAPI Vector3 Vector3Perpendicular(Vector3 v)

// Calculate vector length
RMAPI float Vector3Length(const Vector3 v)

// Calculate vector square length
RMAPI float Vector3LengthSqr(const Vector3 v)

// Calculate two vectors dot product
RMAPI float Vector3DotProduct(Vector3 v1, Vector3 v2)

// Calculate distance between two vectors
RMAPI float Vector3Distance(Vector3 v1, Vector3 v2)

// Calculate square distance between two vectors
RMAPI float Vector3DistanceSqr(Vector3 v1, Vector3 v2)

// Calculate angle between two vectors
RMAPI float Vector3Angle(Vector3 v1, Vector3 v2)

// Negate provided vector (invert direction)
RMAPI Vector3 Vector3Negate(Vector3 v)

// Divide vector by vector
RMAPI Vector3 Vector3Divide(Vector3 v1, Vector3 v2)

// Normalize provided vector
RMAPI Vector3 Vector3Normalize(Vector3 v)

//Calculate the projection of the vector v1 on to v2
RMAPI Vector3 Vector3Project(Vector3 v1, Vector3 v2)

//Calculate the rejection of the vector v1 on to v2
RMAPI Vector3 Vector3Reject(Vector3 v1, Vector3 v2)

// Orthonormalize provided vectors
// Makes vectors normalized and orthogonal to each other
// Gram-Schmidt function implementation
RMAPI void Vector3OrthoNormalize(Vector3 *v1, Vector3 *v2)

// Transforms a Vector3 by a given Matrix
RMAPI Vector3 Vector3Transform(Vector3 v, Matrix mat)

// Transform a vector by quaternion rotation
RMAPI Vector3 Vector3RotateByQuaternion(Vector3 v, Quaternion q)

// Rotates a vector around an axis
RMAPI Vector3 Vector3RotateByAxisAngle(Vector3 v, Vector3 axis, float angle)

// Move Vector towards target
RMAPI Vector3 Vector3MoveTowards(Vector3 v, Vector3 target, float maxDistance)

// Calculate linear interpolation between two vectors
RMAPI Vector3 Vector3Lerp(Vector3 v1, Vector3 v2, float amount)

// Calculate cubic hermite interpolation between two vectors and their tangents
// as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic
RMAPI Vector3 Vector3CubicHermite(Vector3 v1, Vector3 tangent1, Vector3 v2, Vector3 tangent2, float amount)

// Calculate reflected vector to normal
RMAPI Vector3 Vector3Reflect(Vector3 v, Vector3 normal)

// Get min value for each pair of components
RMAPI Vector3 Vector3Min(Vector3 v1, Vector3 v2)

// Get max value for each pair of components
RMAPI Vector3 Vector3Max(Vector3 v1, Vector3 v2)

// Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c)
// NOTE: Assumes P is on the plane of the triangle
RMAPI Vector3 Vector3Barycenter(Vector3 p, Vector3 a, Vector3 b, Vector3 c)

// Projects a Vector3 from screen space into object space
// NOTE: We are avoiding calling other raymath functions despite available
RMAPI Vector3 Vector3Unproject(Vector3 source, Matrix projection, Matrix view)

// Get Vector3 as float array
RMAPI float3 Vector3ToFloatV(Vector3 v)

// Invert the given vector
RMAPI Vector3 Vector3Invert(Vector3 v)

// Clamp the components of the vector between
// min and max values specified by the given vectors
RMAPI Vector3 Vector3Clamp(Vector3 v, Vector3 min, Vector3 max)

// Clamp the magnitude of the vector between two values
RMAPI Vector3 Vector3ClampValue(Vector3 v, float min, float max)

// Check whether two given vectors are almost equal
RMAPI int Vector3Equals(Vector3 p, Vector3 q)

// Compute the direction of a refracted ray
// v: normalized direction of the incoming ray
// n: normalized normal vector of the interface of two optical media
// r: ratio of the refractive index of the medium from where the ray comes
//    to the refractive index of the medium on the other side of the surface
RMAPI Vector3 Vector3Refract(Vector3 v, Vector3 n, float r)


//----------------------------------------------------------------------------------
// Module Functions Definition - Vector4 math
//----------------------------------------------------------------------------------

RMAPI Vector4 Vector4Zero(void)

RMAPI Vector4 Vector4One(void)

RMAPI Vector4 Vector4Add(Vector4 v1, Vector4 v2)

RMAPI Vector4 Vector4AddValue(Vector4 v, float add)

RMAPI Vector4 Vector4Subtract(Vector4 v1, Vector4 v2)

RMAPI Vector4 Vector4SubtractValue(Vector4 v, float add)

RMAPI float Vector4Length(Vector4 v)

RMAPI float Vector4LengthSqr(Vector4 v)

RMAPI float Vector4DotProduct(Vector4 v1, Vector4 v2)

// Calculate distance between two vectors
RMAPI float Vector4Distance(Vector4 v1, Vector4 v2)

// Calculate square distance between two vectors
RMAPI float Vector4DistanceSqr(Vector4 v1, Vector4 v2)

RMAPI Vector4 Vector4Scale(Vector4 v, float scale)

// Multiply vector by vector
RMAPI Vector4 Vector4Multiply(Vector4 v1, Vector4 v2)

// Negate vector
RMAPI Vector4 Vector4Negate(Vector4 v)

// Divide vector by vector
RMAPI Vector4 Vector4Divide(Vector4 v1, Vector4 v2)

// Normalize provided vector
RMAPI Vector4 Vector4Normalize(Vector4 v)

// Get min value for each pair of components
RMAPI Vector4 Vector4Min(Vector4 v1, Vector4 v2)

// Get max value for each pair of components
RMAPI Vector4 Vector4Max(Vector4 v1, Vector4 v2)

// Calculate linear interpolation between two vectors
RMAPI Vector4 Vector4Lerp(Vector4 v1, Vector4 v2, float amount)

// Move Vector towards target
RMAPI Vector4 Vector4MoveTowards(Vector4 v, Vector4 target, float maxDistance)

// Invert the given vector
RMAPI Vector4 Vector4Invert(Vector4 v)

// Check whether two given vectors are almost equal
RMAPI int Vector4Equals(Vector4 p, Vector4 q)


//----------------------------------------------------------------------------------
// Module Functions Definition - Matrix math
//----------------------------------------------------------------------------------

// Compute matrix determinant
RMAPI float MatrixDeterminant(Matrix mat)

// Get the trace of the matrix (sum of the values along the diagonal)
RMAPI float MatrixTrace(Matrix mat)

// Transposes provided matrix
RMAPI Matrix MatrixTranspose(Matrix mat)

// Invert provided matrix
RMAPI Matrix MatrixInvert(Matrix mat)

// Get identity matrix
RMAPI Matrix MatrixIdentity(void)

// Add two matrices
RMAPI Matrix MatrixAdd(Matrix left, Matrix right)

// Subtract two matrices (left - right)
RMAPI Matrix MatrixSubtract(Matrix left, Matrix right)

// Get two matrix multiplication
// NOTE: When multiplying matrices... the order matters!
RMAPI Matrix MatrixMultiply(Matrix left, Matrix right)

// Get translation matrix
RMAPI Matrix MatrixTranslate(float x, float y, float z)

// Create rotation matrix from axis and angle
// NOTE: Angle should be provided in radians
RMAPI Matrix MatrixRotate(Vector3 axis, float angle)

// Get x-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateX(float angle)

// Get y-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateY(float angle)

// Get z-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateZ(float angle)


// Get xyz-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateXYZ(Vector3 angle)

// Get zyx-rotation matrix
// NOTE: Angle must be provided in radians
RMAPI Matrix MatrixRotateZYX(Vector3 angle)

// Get scaling matrix
RMAPI Matrix MatrixScale(float x, float y, float z)

// Get perspective projection matrix
RMAPI Matrix MatrixFrustum(double left, double right, double bottom, double top, double nearPlane, double farPlane)

// Get perspective projection matrix
// NOTE: Fovy angle must be provided in radians
RMAPI Matrix MatrixPerspective(double fovY, double aspect, double nearPlane, double farPlane)

// Get orthographic projection matrix
RMAPI Matrix MatrixOrtho(double left, double right, double bottom, double top, double nearPlane, double farPlane)

// Get camera look-at matrix (view matrix)
RMAPI Matrix MatrixLookAt(Vector3 eye, Vector3 target, Vector3 up)

// Get float array of matrix data
RMAPI float16 MatrixToFloatV(Matrix mat)

//----------------------------------------------------------------------------------
// Module Functions Definition - Quaternion math
//----------------------------------------------------------------------------------

// Add two quaternions
RMAPI Quaternion QuaternionAdd(Quaternion q1, Quaternion q2)

// Add quaternion and float value
RMAPI Quaternion QuaternionAddValue(Quaternion q, float add)

// Subtract two quaternions
RMAPI Quaternion QuaternionSubtract(Quaternion q1, Quaternion q2)

// Subtract quaternion and float value
RMAPI Quaternion QuaternionSubtractValue(Quaternion q, float sub)

// Get identity quaternion
RMAPI Quaternion QuaternionIdentity(void)

// Computes the length of a quaternion
RMAPI float QuaternionLength(Quaternion q)

// Normalize provided quaternion
RMAPI Quaternion QuaternionNormalize(Quaternion q)

// Invert provided quaternion
RMAPI Quaternion QuaternionInvert(Quaternion q)

// Calculate two quaternion multiplication
RMAPI Quaternion QuaternionMultiply(Quaternion q1, Quaternion q2)

// Scale quaternion by float value
RMAPI Quaternion QuaternionScale(Quaternion q, float mul)

// Divide two quaternions
RMAPI Quaternion QuaternionDivide(Quaternion q1, Quaternion q2)

// Calculate linear interpolation between two quaternions
RMAPI Quaternion QuaternionLerp(Quaternion q1, Quaternion q2, float amount)

// Calculate slerp-optimized interpolation between two quaternions
RMAPI Quaternion QuaternionNlerp(Quaternion q1, Quaternion q2, float amount)

// Calculates spherical linear interpolation between two quaternions
RMAPI Quaternion QuaternionSlerp(Quaternion q1, Quaternion q2, float amount)

// Calculate quaternion cubic spline interpolation using Cubic Hermite Spline algorithm
// as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic
RMAPI Quaternion QuaternionCubicHermiteSpline(Quaternion q1, Quaternion outTangent1, Quaternion q2, Quaternion inTangent2, float t)

// Calculate quaternion based on the rotation from one vector to another
RMAPI Quaternion QuaternionFromVector3ToVector3(Vector3 from, Vector3 to)

// Get a quaternion for a given rotation matrix
RMAPI Quaternion QuaternionFromMatrix(Matrix mat)

// Get a matrix for a given quaternion
RMAPI Matrix QuaternionToMatrix(Quaternion q)

// Get rotation quaternion for an angle and axis
// NOTE: Angle must be provided in radians
RMAPI Quaternion QuaternionFromAxisAngle(Vector3 axis, float angle)

// Get the rotation angle and axis for a given quaternion
RMAPI void QuaternionToAxisAngle(Quaternion q, Vector3 *outAxis, float *outAngle)

// Get the quaternion equivalent to Euler angles
// NOTE: Rotation order is ZYX
RMAPI Quaternion QuaternionFromEuler(float pitch, float yaw, float roll)

// Get the Euler angles equivalent to quaternion (roll, pitch, yaw)
// NOTE: Angles are returned in a Vector3 struct in radians
RMAPI Vector3 QuaternionToEuler(Quaternion q)

// Transform a quaternion given a transformation matrix
RMAPI Quaternion QuaternionTransform(Quaternion q, Matrix mat)

// Check whether two given quaternions are almost equal
RMAPI int QuaternionEquals(Quaternion p, Quaternion q)

// Decompose a transformation matrix into its rotational, translational and scaling components
RMAPI void MatrixDecompose(Matrix mat, Vector3 *translation, Quaternion *rotation, Vector3 *scale)
