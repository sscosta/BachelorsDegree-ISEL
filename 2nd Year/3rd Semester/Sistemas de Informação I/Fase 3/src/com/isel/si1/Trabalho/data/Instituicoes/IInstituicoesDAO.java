package com.isel.si1.Trabalho.data.Instituicoes;

import com.isel.si1.Trabalho.model.Instituicoes;

import java.util.List;

public interface IInstituicoesDAO {
    /**
     * Get all products
     *
     * @return List<Product> with all products
     */
    List<Instituicoes> getAllInstituicoes();
}
