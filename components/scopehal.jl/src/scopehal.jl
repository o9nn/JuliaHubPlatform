module scopehal
  using CxxWrap
  using scopehal_jll
  @wrapmodule(scopehal_jll.scopehalwrapper)

  function __init__()
    @initcxx
    TransportStaticInit()
    DriverStaticInit()
  end

  function transportnames()
      v = CxxWrap.StdVector{CxxWrap.StdString}()
      EnumTransports(CxxWrap.CxxRef(v))
      v
  end

  function drivernames()
      v = CxxWrap.StdVector{CxxWrap.StdString}()
      EnumDrivers(CxxWrap.CxxRef(v))
      v
  end
end
