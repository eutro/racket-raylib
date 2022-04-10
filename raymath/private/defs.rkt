(define-raymath
  ["Clamp float value"
   Clamp -> _float
   ([value : _float]
    [min : _float]
    [max : _float])]

  ["Calculate linear interpolation between two floats"
   Lerp -> _float
   ([start : _float]
    [end : _float]
    [amount : _float])]

  ["Normalize input value within input range"
   Normalize -> _float
   ([value : _float]
    [start : _float]
    [end : _float])]

  ["Remap input value within input range to output range"
   Remap -> _float
   ([value : _float]
    [inputStart : _float]
    [inputEnd : _float]
    [outputStart : _float]
    [outputEnd : _float])]

  ;;----------------------------------------------------------------------------------
  ;; Module Functions Definition - Vector2 math
  ;;----------------------------------------------------------------------------------

  ["Vector with components value 0.0f"
   Vector2Zero -> _Vector2
   ()]

  ["Vector with components value 1.0f"
   Vector2One -> _Vector2
   ()]
  
  ["Add two vectors (v1 + v2)"
   Vector2Add -> _Vector2
   ([v1 : _Vector2]
    [v2 : _Vector2])]
  

  ["Add vector and float value"
   Vector2AddValue -> _Vector2
   ([v : _Vector2]
    [add : _float])]

  ["Subtract two vectors (v1 - v2)"
   Vector2Subtract -> _Vector2
   ([v1 : _Vector2]
    [v2 : _Vector2])]

  ["Subtract vector by float value"
   Vector2SubtractValue -> _Vector2
   ([v : _Vector2]
    [sub : _float])]

  ["Calculate vector length"
   Vector2Length -> _float
   ([v : _Vector2])]

  ["Calculate vector square length"
   Vector2LengthSqr -> _float
   ([v : _Vector2])]

  ["Calculate two vectors dot product"
   Vector2DotProduct -> _float
   ([v1 : _Vector2]
    [v2 : _Vector2])]

  ["Calculate distance between two vectors"
   Vector2Distance -> _float
   ([v1 : _Vector2]
    [v2 : _Vector2])]

  ["Calculate angle from two vectors in X-axis"
   Vector2Angle -> _float
   ([v1 : _Vector2]
    [v2 : _Vector2])]

  ["Scale vector (multiply by value)"
   Vector2Scale -> _Vector2
   ([v : _Vector2]
    [scale : _float])]

  ["Multiply vector by vector"
   Vector2Multiply -> _Vector2
   ([v1 : _Vector2]
    [v2 : _Vector2])]

  ["Negate vector"
   Vector2Negate -> _Vector2
   ([v : _Vector2])]

  ["Divide vector by vector"
   Vector2Divide -> _Vector2
   ([v1 : _Vector2]
    [v2 : _Vector2])]

  ["Normalize provided vector"
   Vector2Normalize -> _Vector2
   ([v : _Vector2])]

  ["Calculate linear interpolation between two vectors"
   Vector2Lerp -> _Vector2
   ([v1 : _Vector2]
    [v2 : _Vector2]
    [amount : _float])]

  ["Calculate reflected vector to normal"
   Vector2Reflect -> _Vector2
   ([v : _Vector2]
    [normal : _Vector2])]

  ["Rotate vector by angle"
   Vector2Rotate -> _Vector2
   ([v : _Vector2]
    [angle : _float])]

  ["Move Vector towards target"
   Vector2MoveTowards -> _Vector2
   ([v : _Vector2]
    [target : _Vector2]
    [maxDistance : _float])]
  
  ;;----------------------------------------------------------------------------------
  ;;Module Functions Definition - Vector3 math
  ;;----------------------------------------------------------------------------------

  ["Vector with components value 0.0f"
   Vector3Zero -> _Vector3
   ()]

  ["Vector with components value 1.0f"
   Vector3One -> _Vector3
   ()]

  ["Add two vectors"
   Vector3Add -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Add vector and float value"
   Vector3AddValue -> _Vector3
   ([v : _Vector3]
    [add : _float])]

  ["Subtract two vectors"
   Vector3Subtract -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Subtract vector by float value"
   Vector3SubtractValue -> _Vector3
   ([v : _Vector3]
    [sub : _float])]

  ["Multiply vector by scalar"
   Vector3Scale -> _Vector3
   ([v : _Vector3]
    [scalar : _float])]

  ["Multiply vector by vector"
   Vector3Multiply -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Calculate two vectors cross product"
   Vector3CrossProduct -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Calculate one vector perpendicular vector"
   Vector3Perpendicular -> _Vector3
   ([v : _Vector3])]

  ["Calculate vector length"
   Vector3Length -> _float
   ([v : _Vector3])]

  ["Calculate vector square length"
   Vector3LengthSqr -> _float
   ([v : _Vector3])]

  ["Calculate two vectors dot product"
   Vector3DotProduct -> _float
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Calculate distance between two vectors"
   Vector3Distance -> _float
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Calculate angle between two vectors in XY and XZ"
   Vector3Angle -> _Vector2
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Negate provided vector (invert direction)"
   Vector3Negate -> _Vector3
   ([v : _Vector3])]

  ["Divide vector by vector"
   Vector3Divide -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Normalize provided vector"
   Vector3Normalize -> _Vector3
   ([v : _Vector3])]

  ["Orthonormalize provided vectors"
   "Makes vectors normalized and orthogonal to each other"
   "Gram-Schmidt function implementation"
   Vector3OrthoNormalize -> _void
   ([v1 : (_pointer-to _Vector3)]
    [v2 : (_pointer-to _Vector3)])]

  ["Transforms a Vector3 by a given Matrix"
   Vector3Transform -> _Vector3
   ([v : _Vector3]
    [mat : _Matrix])]

  ["Transform a vector by quaternion rotation"
   Vector3RotateByQuaternion -> _Vector3
   ([v : _Vector3]
    [q : _Quaternion])]

  ["Calculate linear interpolation between two vectors"
   Vector3Lerp -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3]
    [amount : _float])]

  ["Calculate reflected vector to normal"
   Vector3Reflect -> _Vector3
   ([v : _Vector3]
    [normal : _Vector3])]

  ["Get min value for each pair of components"
   Vector3Min -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Get max value for each pair of components"
   Vector3Max -> _Vector3
   ([v1 : _Vector3]
    [v2 : _Vector3])]

  ["Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c)"
   "NOTE: Assumes P is on the plane of the triangle"
   Vector3Barycenter -> _Vector3
   ([p : _Vector3]
    [a : _Vector3]
    [b : _Vector3]
    [c : _Vector3])]
  
  ["Projects a Vector3 from screen space into object space"
   "NOTE: We are avoiding calling other raymath functions despite available"
   Vector3Unproject -> _Vector3
   ([source : _Vector3]
    [projection : _Matrix]
    [view : _Matrix])]

  ["Get Vector3 as float array"
   Vector3ToFloatV -> _float3
   ([v : _Vector3])]

  ;;----------------------------------------------------------------------------------
  ;; Module Functions Definition - Matrix math
  ;;----------------------------------------------------------------------------------

  ["Compute matrix determinant"
   MatrixDeterminant -> _float
   ([mat : _Matrix])]

  ["Get the trace of the matrix (sum of the values along the diagonal)"
   MatrixTrace -> _float
   ([mat : _Matrix])]

  ["Transposes provided matrix"
   MatrixTranspose -> _Matrix
   ([mat : _Matrix])]

  ["Invert provided matrix"
   MatrixInvert -> _Matrix
   ([mat : _Matrix])]

  ["Normalize provided matrix"
   MatrixNormalize -> _Matrix
   ([mat : _Matrix])]

  ["Get identity matrix"
   MatrixIdentity -> _Matrix
   ()]

  ["Add two matrices"
   MatrixAdd -> _Matrix
   ([left : _Matrix]
    [right : _Matrix])]

  ["Subtract two matrices (left - right)"
   MatrixSubtract -> _Matrix
   ([left : _Matrix]
    [right : _Matrix])]

  ["Get two matrix multiplication"
   "NOTE: When multiplying matrices... the order matters!"
   MatrixMultiply -> _Matrix
   ([left : _Matrix]
    [right : _Matrix])]

  ["Get translation matrix"
   MatrixTranslate -> _Matrix
   ([x : _float]
    [y : _float]
    [z : _float])]

  ["Create rotation matrix from axis and angle"
   "NOTE: Angle should be provided in radians"
   MatrixRotate -> _Matrix
   ([axis : _Vector3]
    [angle : _float])]
  
  ["Get x-rotation matrix (angle in radians)"
   MatrixRotateX -> _Matrix
   ([angle : _float])]
  
  ["Get y-rotation matrix (angle in radians)"
   MatrixRotateY -> _Matrix
   ([angle : _float])]
  
  ["Get z-rotation matrix (angle in radians)"
   MatrixRotateZ -> _Matrix
   ([angle : _float])]  

  ["Get xyz-rotation matrix (angles in radians)"
   MatrixRotateXYZ -> _Matrix
   ([ang : _Vector3])]

  ["Get zyx-rotation matrix (angles in radians)"
   MatrixRotateZYX -> _Matrix
   ([ang : _Vector3])]

  ["Get scaling matrix"
   MatrixScale -> _Matrix
   ([x : _float]
    [y : _float]
    [z : _float])]

  ["Get perspective projection matrix"
   MatrixFrustum -> _Matrix
   ([left : _double]
    [right : _double]
    [bottom : _double]
    [top : _double]
    [near : _double]
    [far : _double])]

  ["Get perspective projection matrix"
   "NOTE: Angle should be provided in radians"
   MatrixPerspective -> _Matrix
   ([fovy : _double]
    [aspect : _double]
    [near : _double]
    [far : _double])]

  ["Get orthographic projection matrix"
   MatrixOrtho -> _Matrix
   ([left : _double]
    [right : _double]
    [bottom : _double]
    [top : _double]
    [near : _double]
    [far : _double])]

  ["Get camera look-at matrix (view matrix)"
   MatrixLookAt -> _Matrix
   ([eye : _Vector3]
    [target : _Vector3]
    [up : _Vector3])]

  ["Get float array of matrix data"
   MatrixToFloatV -> _float16
   ([mat : _Matrix])]

  ;;----------------------------------------------------------------------------------
  ;; Module Functions Definition - Quaternion math
  ;;----------------------------------------------------------------------------------

  ["Add two quaternions"
   QuaternionAdd -> _Quaternion
   ([q1 : _Quaternion]
    [q2 : _Quaternion])]

  ["Add quaternion and float value"
   QuaternionAddValue -> _Quaternion
   ([q : _Quaternion]
    [add : _float])]

  ["Subtract two quaternions"
   QuaternionSubtract -> _Quaternion
   ([q1 : _Quaternion]
    [q2 : _Quaternion])]

  ["Subtract quaternion and float value"
   QuaternionSubtractValue -> _Quaternion
   ([q : _Quaternion]
    [sub : _float])]

  ["Get identity quaternion"
   QuaternionIdentity -> _Quaternion
   ()]

  ["Computes the length of a quaternion"
   QuaternionLength -> _float
   ([q : _Quaternion])]

  ["Normalize provided quaternion"
   QuaternionNormalize -> _Quaternion
   ([q : _Quaternion])]

  ["Invert provided quaternion"
   QuaternionInvert -> _Quaternion
   ([q : _Quaternion])]

  ["Calculate two quaternion multiplication"
   QuaternionMultiply -> _Quaternion
   ([q1 : _Quaternion]
    [q2 : _Quaternion])]

  ["Scale quaternion by float value"
   QuaternionScale -> _Quaternion
   ([q : _Quaternion]
    [mul : _float])]

  ["Calculate linear interpolation between two quaternions"
   QuaternionLerp -> _Quaternion
   ([q1 : _Quaternion]
    [q2 : _Quaternion]
    [amount : _float])]

  ["Calculate slerp-optimized interpolation between two quaternions"
   QuaternionNlerp -> _Quaternion
   ([q1 : _Quaternion]
    [q2 : _Quaternion]
    [amount : _float])]

  ["Calculates spherical linear interpolation between two quaternions"
   QuaternionSlerp -> _Quaternion
   ([q1 : _Quaternion]
    [q2 : _Quaternion]
    [amount : _float])]

  ["Calculate quaternion based on the rotation from one vector to another"
   QuaternionFromVector3ToVector3 -> _Quaternion
   ([from : _Vector3]
    [to : _Vector3])]

  ["Get a quaternion for a given rotation matrix"
   QuaternionFromMatrix -> _Quaternion
   ([mat : _Matrix])]

  ["Get a matrix for a given quaternion"
   QuaternionToMatrix -> _Matrix
   ([q : _Quaternion])]

  ["Get rotation quaternion for an angle and axis"
   "NOTE: angle must be provided in radians"
   QuaternionFromAxisAngle -> _Quaternion
   ([axis : _Vector3]
    [angle : _float])]

  ["Get the rotation angle and axis for a given quaternion"
   QuaternionToAxisAngle -> _void
   (values outAxis outAngle)
   ([q : _Quaternion]
    [outAxis : (_ptr o _Vector3)]
    [outAngle : (_ptr o _float)])]

  ["Get the quaternion equivalent to Euler angles"
   "NOTE: Rotation order is ZYX"
   QuaternionFromEuler -> _Quaternion
   ([pitch : _float]
    [yaw : _float]
    [roll : _float])]

  ["Get the Euler angles equivalent to quaternion (roll, pitch, yaw)"
   "NOTE: Angles are returned in a Vector3 struct in radians"
   QuaternionToEuler -> _Vector3
   ([q : _Quaternion])]

  ["Transform a quaternion given a transformation matrix"
   QuaternionTransform -> _Quaternion
   ([q : _Quaternion]
    [mat : _Matrix])])
