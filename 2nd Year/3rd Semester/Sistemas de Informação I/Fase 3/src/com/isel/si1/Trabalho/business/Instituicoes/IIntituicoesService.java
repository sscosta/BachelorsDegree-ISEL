package com.isel.si1.Trabalho.business.Instituicoes;

import com.isel.si1.Trabalho.model.Instituicoes;

import java.util.List;

public interface IIntituicoesService {

    /**
            * Get all Instituicoes
	 *
             * @return List<Instituicao> with all products
	 */
    List<Instituicoes> getAllInstituicoes();
}
