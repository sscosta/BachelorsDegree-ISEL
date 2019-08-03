package com.isel.si1.Trabalho.model;

import java.math.BigDecimal;

public class Product {

	
	private String ref;
	private String name;
	private String description;
	private BigDecimal price;
	private BigDecimal volume;
	/**
	 * @return the ref
	 */
	public String getRef() {
		return ref;
	}
	/**
	 * @param ref the ref to set
	 */
	public void setRef(String ref) {
		this.ref = ref;
	}
	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}
	/**
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}
	/**
	 * @param description the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}
	/**
	 * @return the price
	 */
	public BigDecimal getPrice() {
		return price;
	}
	/**
	 * @param price the price to set
	 */
	public void setPrice(BigDecimal price) {
		this.price = price;
	}
	/**
	 * @return the volume
	 */
	public BigDecimal getVolume() {
		return volume;
	}
	/**
	 * @param volume the volume to set
	 */
	public void setVolume(BigDecimal volume) {
		this.volume = volume;
	}
	
	@Override
	public String toString() {
		return "Product [ref=" + ref + ", name=" + name + ", description=" + description + ", price=" + price
				+ ", volume=" + volume + "]";
	}
	
	
}
