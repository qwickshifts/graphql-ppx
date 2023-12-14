let assign_typename: (Js.Json.t, string) => Js.Json.t = (
  [%raw {| (obj, typename) => { obj.__typename = typename; return obj } |}]:
    (Js.Json.t, string) => Js.Json.t
);

let%private clone: Js.Dict.t('a) => Js.Dict.t('a) =
  a => Obj.magic(Js.Obj.assign(Obj.magic(Js.Obj.empty()), Obj.magic(a)));

let rec deepMerge = (json1: Js.Json.t, json2: Js.Json.t) =>
  switch (
    (
      Obj.magic(json1) == Js.null,
      Js.Array2.isArray(json1),
      Js.typeof(json1) == "object",
    ),
    (
      Obj.magic(json2) == Js.null,
      Js.Array2.isArray(json2),
      Js.typeof(json2) == "object",
    ),
  ) {
  | ((_, true, _), (_, true, _)) => (
      Obj.magic(
        Js.Array2.mapi(
          Obj.magic(json1),
          (el1, idx) => {
            let el2 = Js.Array2.unsafe_get(Obj.magic(json2), idx);
            Js.typeof(el2) == "object" ? deepMerge(el1, el2) : el2;
          },
        ),
      ): Js.Json.t
    )
  | ((false, false, true), (false, false, true)) =>
    let obj1 = clone(Obj.magic(json1));
    let obj2 = Obj.magic(json2);
    Js.Array2.forEach(
      Js.Dict.keys(obj2),
      key => {
        let existingVal: Js.Json.t = Js.Dict.unsafeGet(obj1, key);
        let newVal: Js.Json.t = Js.Dict.unsafeGet(obj2, key);
        Js.Dict.set(
          obj1,
          key,
          Js.typeof(existingVal) != "object"
            ? newVal : Obj.magic(deepMerge(existingVal, newVal)),
        );
      },
    );
    Obj.magic(obj1);
  | ((_, _, _), (_, _, _)) => json2
  };
