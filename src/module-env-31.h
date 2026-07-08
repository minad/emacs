  /* Add module environment functions newly added in Emacs 31 here.
     Before Emacs 31 is released, remove this comment and start
     module-env-32.h on the master branch.  */

  /* Get pointer to the pixel buffer of CANVAS.  The buffer is in row
     major order and has the size width * height.  The pixel format is
     ARGB32.  The pointer will be valid as long as CANVAS is alive,
     and as long as its dimensions have not been changed.
     Return NULL in case error.   */

  uint32_t* (*canvas_data) (emacs_env *env, emacs_value canvas)
    EMACS_ATTRIBUTE_NONNULL(1);
