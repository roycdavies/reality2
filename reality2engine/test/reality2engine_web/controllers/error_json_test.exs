defmodule Reality2engineWeb.ErrorJSONTest do
  use Reality2engineWeb.ConnCase, async: true

  test "renders 404" do
    assert Reality2engineWeb.ErrorJSON.render("404.json", %{}) == %{errors: %{detail: "Not Found"}}
  end

  test "renders 500" do
    assert Reality2engineWeb.ErrorJSON.render("500.json", %{}) ==
             %{errors: %{detail: "Internal Server Error"}}
  end
end
