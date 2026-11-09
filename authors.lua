-- authors.lua
-- Convierte el bloque `author` de authors.yml (lista de mapas con name,
-- affiliations, corresponding...) en una lista de autores con superíndices de
-- afiliación, y añade tras el título el bloque de afiliaciones y la persona
-- de correspondencia. Las plantillas por defecto de pandoc no saben imprimir
-- autores definidos como mapas (en PDF/HTML/Word aparece "true").
--
-- Uso (en el YAML del Rmd, dentro de pandoc_args de cada formato):
--   pandoc_args: ["--metadata-file=authors.yml", "--lua-filter=authors.lua"]

local stringify = pandoc.utils.stringify

local function str(x)
  if x == nil then return "" end
  return stringify(x)
end

function Pandoc(doc)
  local meta = doc.meta
  if meta.author == nil or meta.affiliations == nil then
    return nil -- nada que hacer si no hay authors.yml cargado
  end

  -- id de afiliación -> número
  local aff_number, aff_list = {}, {}
  for _, aff in ipairs(meta.affiliations) do
    aff_number[str(aff.id)] = str(aff.number)
    aff_list[#aff_list + 1] = aff
  end

  -- línea de autores con superíndices
  local authors = pandoc.List()
  local corresponding = nil
  for _, a in ipairs(meta.author) do
    local marks = {}
    if a.affiliations ~= nil then
      for _, r in ipairs(a.affiliations) do
        marks[#marks + 1] = aff_number[str(r.ref)] or "?"
      end
    end
    local sup = table.concat(marks, ",")
    if str(a.corresponding) == "true" then
      corresponding = a
      sup = (sup ~= "" and (sup .. ",") or "") .. "*"
    end
    local inl = pandoc.Inlines({ pandoc.Str(str(a.name)) })
    if sup ~= "" then
      inl:insert(pandoc.Superscript({ pandoc.Str(sup) }))
    end
    authors:insert(pandoc.MetaInlines(inl))
  end
  meta.author = pandoc.MetaList(authors)

  -- bloque de afiliaciones (+ correspondencia) justo después del título
  local lines = pandoc.Inlines({})
  for i, aff in ipairs(aff_list) do
    if i > 1 then lines:insert(pandoc.LineBreak()) end
    lines:insert(pandoc.Superscript({ pandoc.Str(str(aff.number)) }))
    lines:insert(pandoc.Space())
    lines:insert(pandoc.Str(
      str(aff.name) .. ", " .. str(aff.city) .. ", " .. str(aff.country) .. "."))
  end
  if corresponding ~= nil and str(corresponding.email) ~= "" then
    lines:insert(pandoc.LineBreak())
    lines:insert(pandoc.Superscript({ pandoc.Str("*") }))
    lines:insert(pandoc.Space())
    lines:insert(pandoc.Str(
      "Corresponding author: " .. str(corresponding.name) .. " (" ..
      str(corresponding.email) .. ")."))
  end

  local block = pandoc.Para(lines)
  local blocks = pandoc.List({ block })
  if FORMAT:match("latex") then
    blocks = pandoc.List({
      pandoc.RawBlock("latex", "\\begingroup\\footnotesize"),
      block,
      pandoc.RawBlock("latex", "\\endgroup"),
    })
  end

  local new_blocks = pandoc.List()
  new_blocks:extend(blocks)
  new_blocks:extend(doc.blocks)
  return pandoc.Pandoc(new_blocks, meta)
end
