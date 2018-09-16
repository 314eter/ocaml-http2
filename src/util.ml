open S

module Make (C : C) = struct
  module Let_syntax = struct
    let bind x ~f = C.bind x f
    let map x ~f = C.map f x
  end

  module R = struct
    let return x = C.return (Ok x)
    let fail e = C.return (Error e)

    module Let_syntax = struct
      let bind x ~f =
        match%bind x with
        | Ok x -> f x
        | Error e -> fail e

      let map x ~f =
        match%map x with
        | Ok x -> Ok (f x)
        | Error e -> Error e
    end
  end
end
