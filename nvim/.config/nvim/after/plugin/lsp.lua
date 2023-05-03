local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
end)

lsp.ensure_installed({
  'lua_ls',
  'gopls',
  'clangd',
  'tsserver',
  'eslint',
  'emmet_ls',
  'rust_analyzer',
})

local cmp = require('cmp')
local cmp_action = require('lsp-zero').cmp_action()

cmp.setup({
  mapping = {
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-f>'] = cmp_action.luasnip_jump_forward(),
    ['<C-b>'] = cmp_action.luasnip_jump_backward(),
    ['<C-j>'] = cmp.mapping.select_next_item(),
    ['<C-k>'] = cmp.mapping.select_prev_item(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ["<Tab>"] = cmp.mapping(function(fallback)
      if vim.fn.pumvisible() == 1 then
        feedkey("<C-n>", "n")
      elseif cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end, {
      "i",
    }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if vim.fn.pumvisible() == 1 then
        feedkey("<C-p>", "n")
      elseif cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end, {
      "i",
    }),
  },
})

require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

lsp.setup()
